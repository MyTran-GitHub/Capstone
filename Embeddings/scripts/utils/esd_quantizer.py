"""
ESD Quantizer for converting quantized codes to continuous vector embeddings.

Based on the original ESD dataset implementation. The quantizer converts
uint16 codes from the ESD GeoTIFF into 6-dimensional continuous vectors
in the range [-1, 1].

Reference: https://github.com/google-research/earth-data-tools
"""

from __future__ import annotations
from typing import List, Optional
import numpy as np

try:
    import torch
    from torch.nn import Module
    from torch import Tensor, int32
    TORCH_AVAILABLE = True
except ImportError:
    TORCH_AVAILABLE = False


class Quantizer:
    """
    Converts ESD quantized codes to continuous vector embeddings.
    
    The ESD dataset uses product quantization to compress 6-dimensional vectors
    into single uint16 codes. This class performs the inverse operation.
    
    Args:
        levels: Number of quantization levels for each dimension [8,8,8,5,5,5]
        use_torch: Whether to use PyTorch (faster) or NumPy
    """
    
    def __init__(
        self,
        levels: List[int] = [8, 8, 8, 5, 5, 5],
        use_torch: bool = True,
    ):
        self.levels = np.array(levels, dtype=np.int32)
        self.n_dims = len(levels)
        
        # Compute basis for converting flat indices to level indices
        self.basis = np.cumprod(np.array([1] + levels[:-1]), dtype=np.int32)
        
        # Determine computation backend
        self.use_torch = use_torch and TORCH_AVAILABLE
        
        if self.use_torch:
            self._levels_torch = torch.tensor(levels, dtype=torch.int32)
            self._basis_torch = torch.tensor(self.basis, dtype=torch.int32)
    
    def _scale_and_shift_inverse(self, level_indices: np.ndarray) -> np.ndarray:
        """
        Convert level indices to continuous values in [-1, 1].
        
        For quantization levels [8,8,8,5,5,5]:
        - Dimensions 0-2: 8 levels → indices 0-7 → values -1 to 1
        - Dimensions 3-5: 5 levels → indices 0-4 → values -1 to 1
        """
        half_width = self.levels / 2.0
        return (level_indices - half_width) / half_width
    
    def indices_to_level_indices(self, indices: np.ndarray) -> np.ndarray:
        """
        Convert flat indices to per-dimension level indices.
        
        Args:
            indices: Array of shape (..., n_codes) with uint16 values
            
        Returns:
            Array of shape (..., n_codes, 6) with level indices for each dimension
        """
        # Add dimension for broadcasting
        indices = np.expand_dims(indices, axis=-1)  # (..., n_codes, 1)
        
        # Decompose flat index into level indices
        level_indices = (indices // self.basis) % self.levels  # (..., n_codes, 6)
        
        return level_indices
    
    def indices_to_codes(self, indices: np.ndarray) -> np.ndarray:
        """
        Convert quantized indices to continuous vector embeddings.
        
        Args:
            indices: Array of shape (..., n_codes) with uint16/int32 values
            
        Returns:
            Array of shape (..., n_codes, 6) with float32 values in [-1, 1]
            
        Example:
            >>> quantizer = Quantizer()
            >>> codes = np.array([[1234, 5678, 9012]], dtype=np.uint16)  # (1, 3)
            >>> vectors = quantizer.indices_to_codes(codes)  # (1, 3, 6)
        """
        indices = np.asarray(indices, dtype=np.int32)
        level_indices = self.indices_to_level_indices(indices)
        codes = self._scale_and_shift_inverse(level_indices)
        return codes.astype(np.float32)
    
    def codes_to_indices(self, codes: np.ndarray) -> np.ndarray:
        """
        Convert continuous vectors to quantized indices (inverse operation).
        
        This is mainly for reference - not used in the embedding extraction pipeline.
        
        Args:
            codes: Array of shape (..., n_codes, 6) with float32 values in [-1, 1]
            
        Returns:
            Array of shape (..., n_codes) with uint16 values
        """
        # Scale and shift to get level indices
        half_width = self.levels / 2.0
        level_indices = np.round(codes * half_width + half_width).astype(np.int32)
        
        # Clip to valid range
        level_indices = np.clip(level_indices, 0, self.levels - 1)
        
        # Convert level indices to flat indices
        indices = np.sum(level_indices * self.basis, axis=-1)
        
        return indices.astype(np.uint16)


# Only define PyTorch version if torch is available
if TORCH_AVAILABLE:
    class QuantizerTorch(Module):
        """
        PyTorch version of the Quantizer for GPU acceleration.
        
        Use this if you have a GPU and need to process large batches of embeddings.
        """
        
        def __init__(
            self,
            levels: List[int] = [8, 8, 8, 5, 5, 5],
        ):
            super().__init__()
            
            _levels = torch.tensor(levels, dtype=int32)
            self.register_buffer("_levels", _levels, persistent=False)
            
            _basis = torch.cumprod(torch.tensor([1] + levels[:-1]), dim=0, dtype=int32)
            self.register_buffer("_basis", _basis, persistent=False)
        
        def _scale_and_shift_inverse(self, zhat):
            half_width = self._levels // 2
            return (zhat - half_width) / half_width
        
        def indices_to_level_indices(self, indices):
            """Converts indices to indices at each level."""
            indices = indices.unsqueeze(-1)  # Add dimension for broadcasting
            codes_non_centered = (indices // self._basis) % self._levels
            return codes_non_centered
        
        def indices_to_codes(self, indices):
            """
            Inverse of codes_to_indices - convert quantized indices to vectors.
            
            Args:
                indices: Tensor of shape (..., n_codes) with int32 values
                
            Returns:
                Tensor of shape (..., n_codes, 6) with float32 values
            """
            level_indices = self.indices_to_level_indices(indices)
            codes = self._scale_and_shift_inverse(level_indices)
            return codes
else:
    # Placeholder when PyTorch not available
    QuantizerTorch = None


def load_quantizer(model_path: Optional[str] = None, use_torch: bool = False) -> Quantizer:
    """
    Load a pre-trained quantizer.
    
    Args:
        model_path: Path to quantizer weights (e.g., "ESD_quantizer.pth")
                   If None, returns an initialized quantizer with default levels
        use_torch: Whether to use PyTorch version
        
    Returns:
        Initialized Quantizer instance
        
    Note:
        The quantizer levels are fixed for the ESD dataset, so even without
        loading weights, the quantizer works correctly for ESD data.
    """
    if use_torch and TORCH_AVAILABLE and QuantizerTorch is not None:
        quantizer = QuantizerTorch()
        if model_path is not None:
            state_dict = torch.load(model_path, map_location="cpu")
            quantizer.load_state_dict(state_dict)
        return quantizer
    else:
        if use_torch and not TORCH_AVAILABLE:
            print("Warning: PyTorch not available, falling back to NumPy implementation")
        quantizer = Quantizer(use_torch=False)
        # For NumPy version, weights are just the levels and basis (already initialized)
        return quantizer


# ============================================================================
# UTILITY FUNCTIONS
# ============================================================================

def dequantize_esd_tile(
    codes: np.ndarray,
    quantizer: Optional[Quantizer] = None,
) -> np.ndarray:
    """
    Dequantize an entire ESD tile from codes to vectors.
    
    Args:
        codes: Array of shape (12, height, width) with uint16 codes
        quantizer: Quantizer instance (creates new one if None)
        
    Returns:
        Array of shape (12, 6, height, width) with float32 vectors
        
    Example:
        >>> codes = np.random.randint(0, 10000, (12, 3600, 3600), dtype=np.uint16)
        >>> vectors = dequantize_esd_tile(codes)
        >>> vectors.shape
        (12, 6, 3600, 3600)
    """
    if quantizer is None:
        quantizer = Quantizer(use_torch=False)
    
    n_months, height, width = codes.shape
    vectors = np.empty([n_months, 6, height, width], dtype=np.float32)
    
    # Process row by row to avoid memory issues
    for row in range(height):
        row_codes = codes[:, row, :]  # (12, width)
        row_vectors = quantizer.indices_to_codes(row_codes)  # (12, width, 6)
        vectors[:, :, row, :] = row_vectors.transpose(0, 2, 1)  # (12, 6, width)
    
    return vectors


def dequantize_pixel(
    codes: np.ndarray,
    quantizer: Optional[Quantizer] = None,
) -> np.ndarray:
    """
    Dequantize a single pixel's codes to vectors.
    
    Args:
        codes: Array of shape (12,) with uint16 codes for 12 months
        quantizer: Quantizer instance (creates new one if None)
        
    Returns:
        Array of shape (12, 6) or (72,) flattened - 6D vector per month
        
    Example:
        >>> codes = np.array([1234, 5678, 9012, 1111, 2222, 3333, 4444, 5555, 6666, 7777, 8888, 9999], dtype=np.uint16)
        >>> vectors = dequantize_pixel(codes)
        >>> vectors.shape
        (12, 6)
    """
    if quantizer is None:
        quantizer = Quantizer(use_torch=False)
    
    codes = np.asarray(codes, dtype=np.int32)
    if codes.ndim == 1:
        codes = codes.reshape(1, -1)  # (1, 12)
    
    vectors = quantizer.indices_to_codes(codes)  # (1, 12, 6)
    return vectors.squeeze(0)  # (12, 6)


# ============================================================================
# TESTING / DEMO
# ============================================================================

if __name__ == "__main__":
    print("="*80)
    print("ESD Quantizer Demo")
    print("="*80)
    
    # Create quantizer
    quantizer = Quantizer(use_torch=False)
    print(f"\nQuantizer initialized with levels: {quantizer.levels}")
    print(f"Basis values: {quantizer.basis}")
    print(f"Output dimensions: {quantizer.n_dims}")
    
    # Test with sample codes
    print("\n" + "-"*80)
    print("Test 1: Single pixel (12 months)")
    print("-"*80)
    sample_codes = np.array([1234, 5678, 9012, 1111, 2222, 3333, 4444, 5555, 6666, 7777, 8888, 9999], dtype=np.uint16)
    print(f"Input codes shape: {sample_codes.shape}")
    print(f"Sample codes: {sample_codes[:3]}...")
    
    vectors = dequantize_pixel(sample_codes, quantizer)
    print(f"\nOutput vectors shape: {vectors.shape}")
    print(f"Value range: [{vectors.min():.3f}, {vectors.max():.3f}]")
    print(f"Sample vectors (first month, all 6 dims): {vectors[0]}")
    
    # Test batch processing
    print("\n" + "-"*80)
    print("Test 2: Multiple pixels (batch)")
    print("-"*80)
    batch_codes = np.random.randint(0, 10000, (5, 12), dtype=np.uint16)
    print(f"Input batch shape: {batch_codes.shape} (5 pixels × 12 months)")
    
    batch_vectors = quantizer.indices_to_codes(batch_codes)
    print(f"Output batch shape: {batch_vectors.shape} (5 pixels × 12 months × 6 dims)")
    print(f"Total dimensions per pixel: {batch_vectors.shape[1] * batch_vectors.shape[2]}")
    
    # Verify round-trip
    print("\n" + "-"*80)
    print("Test 3: Round-trip (codes → vectors → codes)")
    print("-"*80)
    original_codes = np.array([1000, 2000, 3000, 4000, 5000, 6000], dtype=np.uint16)
    vectors = quantizer.indices_to_codes(original_codes.reshape(1, -1))
    recovered_codes = quantizer.codes_to_indices(vectors)
    
    print(f"Original codes: {original_codes}")
    print(f"Recovered codes: {recovered_codes.flatten()}")
    print(f"Match: {np.allclose(original_codes, recovered_codes.flatten())}")
    
    print("\n" + "="*80)
    print("Demo complete!")
    print("="*80)
