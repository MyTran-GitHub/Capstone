#!/usr/bin/env python3
"""
Compute embeddings for Landsat chips using Prithvi foundation model.

This script loads Landsat imagery chips and extracts embeddings using the
Prithvi geospatial foundation model from IBM/NASA.

Usage:
    python compute_embeddings.py --model prithvi --output embeddings.parquet
"""

import argparse
from pathlib import Path
from typing import List

import numpy as np
import pandas as pd
import torch
from torch.utils.data import Dataset, DataLoader
from torchvision import transforms
from PIL import Image
import rasterio
from tqdm import tqdm


class LandsatChipDataset(Dataset):
    """Dataset for loading Landsat chips."""
    
    def __init__(self, manifest_csv: str, transform=None):
        self.manifest = pd.read_csv(manifest_csv)
        self.transform = transform
    
    def __len__(self):
        return len(self.manifest)
    
    def __getitem__(self, idx):
        row = self.manifest.iloc[idx]
        chip_path = row['chip_path']
        
        # Load GeoTIFF
        with rasterio.open(chip_path) as src:
            # Read all bands (typically 6 for Landsat)
            img = src.read()  # Shape: (bands, height, width)
            
            # Normalize to 0-1 range
            img = np.clip(img, 0, 1)
            
            # Convert to tensor
            img = torch.from_numpy(img).float()
        
        if self.transform:
            img = self.transform(img)
        
        return {
            'image': img,
            'unit': row['unit'],
            'lat': row['LATITUDE'],
            'lon': row['LONGITUDE']
        }


def load_prithvi_model(device='cuda'):
    """
    Load Prithvi model from HuggingFace or local checkpoint.
    
    Note: Prithvi model may require specific installation.
    Alternative: Use a simpler pretrained vision transformer.
    """
    try:
        # Try to import Prithvi (if available)
        from prithvi import PrithviEncoder
        model = PrithviEncoder.from_pretrained('ibm-nasa-geospatial/Prithvi-100M')
        model = model.to(device)
        model.eval()
        return model
    except ImportError:
        print("Warning: Prithvi not available. Using ResNet50 as fallback.")
        from torchvision.models import resnet50, ResNet50_Weights
        
        # Load pretrained ResNet50 and remove final classification layer
        weights = ResNet50_Weights.IMAGENET1K_V2
        model = resnet50(weights=weights)
        
        # Modify first conv to accept 6 channels (Landsat bands)
        original_conv = model.conv1
        model.conv1 = torch.nn.Conv2d(
            6, 64, kernel_size=7, stride=2, padding=3, bias=False
        )
        
        # Initialize new conv with averaged ImageNet weights
        with torch.no_grad():
            model.conv1.weight[:, :3, :, :] = original_conv.weight
            model.conv1.weight[:, 3:, :, :] = original_conv.weight
        
        # Remove classification head (use penultimate layer embeddings)
        model = torch.nn.Sequential(*list(model.children())[:-1])
        
        model = model.to(device)
        model.eval()
        return model


def compute_embeddings(
    manifest_csv: str,
    model_name: str = 'prithvi',
    batch_size: int = 8,
    device: str = None
) -> pd.DataFrame:
    """
    Compute embeddings for all chips in manifest.
    
    Returns:
        DataFrame with columns: unit, lat, lon, embedding (list)
    """
    if device is None:
        device = 'cuda' if torch.cuda.is_available() else 'cpu'
    
    print(f"Using device: {device}")
    
    # Load model
    print(f"Loading {model_name} model...")
    model = load_prithvi_model(device)
    
    # Prepare dataset
    dataset = LandsatChipDataset(manifest_csv)
    dataloader = DataLoader(
        dataset,
        batch_size=batch_size,
        shuffle=False,
        num_workers=2
    )
    
    # Compute embeddings
    embeddings_list = []
    
    with torch.no_grad():
        for batch in tqdm(dataloader, desc="Computing embeddings"):
            images = batch['image'].to(device)
            
            # Forward pass
            emb = model(images)
            
            # Flatten spatial dimensions if needed
            if len(emb.shape) == 4:  # (batch, channels, h, w)
                emb = torch.nn.functional.adaptive_avg_pool2d(emb, (1, 1))
                emb = emb.view(emb.size(0), -1)
            elif len(emb.shape) == 3:  # (batch, channels, spatial)
                emb = emb.mean(dim=2)
            
            emb = emb.cpu().numpy()
            
            # Store results
            for i in range(len(batch['unit'])):
                embeddings_list.append({
                    'unit': batch['unit'][i],
                    'LATITUDE': batch['lat'][i].item(),
                    'LONGITUDE': batch['lon'][i].item(),
                    'embedding': emb[i].tolist()
                })
    
    df = pd.DataFrame(embeddings_list)
    print(f"Computed {len(df)} embeddings, dimension: {len(df['embedding'].iloc[0])}")
    
    return df


def main():
    parser = argparse.ArgumentParser(description='Compute embeddings for Landsat chips')
    parser.add_argument('--manifest', type=str, default='data/imagery/chip_manifest.csv',
                       help='Path to chip manifest CSV')
    parser.add_argument('--model', type=str, default='prithvi',
                       choices=['prithvi', 'resnet'],
                       help='Model to use for embeddings')
    parser.add_argument('--output', type=str, default='data/processed_data/embeddings.parquet',
                       help='Output file for embeddings')
    parser.add_argument('--batch-size', type=int, default=8,
                       help='Batch size for inference')
    parser.add_argument('--device', type=str, default=None,
                       help='Device (cuda/cpu, auto-detect if None)')
    
    args = parser.parse_args()
    
    # Compute embeddings
    embeddings_df = compute_embeddings(
        args.manifest,
        model_name=args.model,
        batch_size=args.batch_size,
        device=args.device
    )
    
    # Save
    output_path = Path(args.output)
    output_path.parent.mkdir(parents=True, exist_ok=True)
    
    embeddings_df.to_parquet(output_path, index=False)
    print(f"Embeddings saved to {output_path}")
    
    # Also save as CSV for R compatibility
    csv_path = output_path.with_suffix('.csv')
    
    # Flatten embeddings to columns
    emb_array = np.array(embeddings_df['embedding'].tolist())
    emb_cols = {f'emb_{i}': emb_array[:, i] for i in range(emb_array.shape[1])}
    
    df_flat = pd.DataFrame({
        'unit': embeddings_df['unit'],
        'LATITUDE': embeddings_df['LATITUDE'],
        'LONGITUDE': embeddings_df['LONGITUDE'],
        **emb_cols
    })
    
    df_flat.to_csv(csv_path, index=False)
    print(f"Flattened embeddings saved to {csv_path}")


if __name__ == '__main__':
    main()
