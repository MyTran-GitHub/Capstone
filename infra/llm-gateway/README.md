# LLM Gateway: Enable Multiple Models for All Clients

This folder provides a turnkey gateway exposing an OpenAI-compatible API served locally (or on a server). It uses LiteLLM as a proxy so any SDK that speaks to OpenAI can call approved models through a single base URL.

Defaults:
- Default model: OpenAI `gpt-4o-mini`
- Also available: Anthropic `claude-3-5-haiku-latest`

Note: If you originally requested "Claude Haiku 4.5", this gateway now supports multiple options; you can use either by specifying `model` or rely on the default.

## Quick Start

1) Prerequisites
- Docker + Docker Compose installed
- For default model: OpenAI API key (`OPENAI_API_KEY`)
- Optional: Anthropic API key (`ANTHROPIC_API_KEY`) to use Claude

2) Set your key(s) (macOS/Linux)
```bash
export OPENAI_API_KEY=sk-openai-...       # required for default gpt-4o-mini
export ANTHROPIC_API_KEY=sk-ant-...       # optional, if you want Claude too
```

3) Start the gateway
```bash
cd infra/llm-gateway
docker compose up -d
```

4) Sanity check (OpenAI-compatible Chat Completions)
```bash
curl -s http://localhost:4000/v1/chat/completions \
  -H 'Content-Type: application/json' \
  -d '{
    "model": "gpt-4o-mini",
    "messages": [{"role": "user", "content": "Say hello in one sentence."}]
  }' | jq '.choices[0].message.content'
```

If you omit the model, the proxy defaults to `gpt-4o-mini` (per `general_settings.default_model`).

## How Clients Use It

Point your clients to `http://<host>:4000` and use standard OpenAI SDKs.

Python (openai>=1.0)
```python
from openai import OpenAI
client = OpenAI(base_url="http://localhost:4000/v1", api_key="dummy")

resp = client.chat.completions.create(
  model="gpt-4o-mini",  # or omit to use the default
    messages=[{"role": "user", "content": "Give me 3 bullet points about Haiku."}],
)
print(resp.choices[0].message.content)
```

Node.js (openai npm)
```javascript
import OpenAI from "openai";
const client = new OpenAI({ baseURL: "http://localhost:4000/v1", apiKey: "dummy" });

const resp = await client.chat.completions.create({
  model: "gpt-4o-mini", // or omit to use the default
  messages: [{ role: "user", content: "One-sentence summary of this gateway." }],
});
console.log(resp.choices[0].message.content);
```

cURL without specifying a model (uses default)
```bash
curl -s http://localhost:4000/v1/chat/completions \
  -H 'Content-Type: application/json' \
  -d '{
    "messages": [{"role": "user", "content": "Default model check: reply 'ok'."}]
  }'
```

## Policy: "Enable for All Clients"

- Default Model: The proxy sets `gpt-4o-mini` as `general_settings.default_model`, so any client that omits `model` uses it automatically.
- Allowed Models: The config restricts `allowed_models` to the approved list (`gpt-4o-mini`, `claude-3-5-haiku`). Add more as needed.
- Aliases: The alias `gpt-mini` resolves to `gpt-4o-mini`, and `haiku` resolves to `claude-3-5-haiku`.

## Changing the Model Version

Edit `litellm_config.yaml` and replace:
- `openai/gpt-4o-mini` with another OpenAI model or a pinned version.
- `anthropic/claude-3-5-haiku-latest` with a specific pinned version (e.g., `anthropic/claude-3-5-haiku-20241022`).

Then reload:
```bash
docker compose restart litellm-proxy
```

## Alternatives (If you prefer not to run a gateway)

- Anthropic Console (Teams)
  - Grant team-wide API access and distribute the Anthropic API key via a secrets manager.
  - In code, call Anthropic directly with the `messages` API and set `model: "claude-3-5-haiku-latest"`.

- AWS Bedrock (Org-wide)
  - In the AWS console, enable model access for Claude 3.5 Haiku in Bedrock.
  - Attach `bedrock:InvokeModel` permissions to the roles used by your services.
  - Use the Bedrock runtime SDK; set the model ID to the Claude Haiku 3.5 identifier for your region.

## Troubleshooting

- 401/403 errors: Ensure `ANTHROPIC_API_KEY` is exported in your shell or provided in your deployment environment.
- Timeouts: Check networking from your host to Anthropic endpoints; increase client timeouts.
- Model mismatch: Update the config and restart if you require a specific model suffix.

## Security

- Do not commit secrets.
- If deploying beyond localhost, secure the proxy behind your API gateway or add auth middleware.
