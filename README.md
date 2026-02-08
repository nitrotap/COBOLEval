# COBOLEval: LLM Evaluation for COBOL

[![CI](https://github.com/bloopai/COBOLEval/actions/workflows/ci.yml/badge.svg)](https://github.com/bloopai/COBOLEval/actions/workflows/ci.yml)
[![Python 3.10+](https://img.shields.io/badge/python-3.10+-blue.svg)](https://www.python.org/downloads/)
[![Rust](https://img.shields.io/badge/rust-1.70+-orange.svg)](https://www.rust-lang.org/)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

COBOLEval is a benchmark dataset for evaluating the code generation abilities of Large Language Models on the COBOL programming language. It is a transpilation of the widely-used [HumanEval](https://github.com/openai/human-eval) benchmark from Python into COBOL.

This repository contains:
- **Python to COBOL transpiler** (Rust) - Converts HumanEval tasks to COBOL
- **Evaluation harness** (Python) - Tests LLM-generated COBOL code
- **COBOLEval dataset** - 146 COBOL programming tasks

## Quick Start

### Using Docker (Recommended)

```bash
# Build the image
docker build -t coboleval .

# Run evaluation with OpenAI
docker run -e OPENAI_API_KEY=$OPENAI_API_KEY coboleval \
    python -m scripts.generate openai --model-name gpt-4
```

### Manual Installation

#### Prerequisites

1. **GnuCOBOL 3.2.0+** - Required for COBOL compilation
   ```bash
   # Ubuntu/Debian
   sudo apt-get install gnucobol

   # macOS
   brew install gnucobol

   # Verify installation
   cobc -v  # Should show version 3.2.0 or higher
   ```

2. **Python 3.10+**

3. **Rust** (only needed for transpiler development)
   ```bash
   curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
   ```

#### Installation

```bash
# Clone the repository
git clone https://github.com/bloopai/COBOLEval.git
cd COBOLEval

# Create virtual environment
python -m venv .venv
source .venv/bin/activate  # or `.venv\Scripts\activate` on Windows

# Install Python dependencies
pip install -e ".[dev]"

# Copy environment template
cp .env.example .env
# Edit .env with your API keys
```

## Usage

> **Security Warning**: This program executes untrusted model-generated code. Users are strongly encouraged to run evaluations in a sandboxed environment. The execution call in `evaluation.py` is deliberately commented out to ensure users read this warning.

### Generate Completions

#### OpenAI Models

```bash
# Using CLI
python -m scripts.generate openai --model-name gpt-4 --samples-per-task 1

# With temperature
python -m scripts.generate openai --model-name gpt-4 --temperature 0.7 --samples-per-task 10
```

#### Huggingface Models

```bash
# Completion mode
python -m scripts.generate huggingface --model-name codellama/CodeLlama-7b-hf

# Infill mode (for models with fill-in-the-middle capability)
python -m scripts.generate huggingface-infill \
    --model-name bigcode/starcoder \
    --prefix-token "<fim_prefix>" \
    --suffix-token "<fim_suffix>" \
    --middle-token "<fim_middle>"
```

#### From Pre-generated JSONL

```bash
python -m scripts.generate json --jsonl-path ./completions.jsonl
```

### Evaluate Results

```bash
# Evaluate a single model
python -m scripts.evaluate_functional_correctness evaluate \
    --pred-path preds/gpt-4 \
    --k "1,10,100"

# Batch evaluate multiple models
python -m scripts.evaluate_functional_correctness batch \
    --pred-paths '["preds/gpt-4", "preds/gpt-3.5-turbo"]'
```

### Run the Transpiler

To regenerate the COBOLEval dataset from HumanEval:

```bash
# Build the transpiler
cargo build --release

# Run with default paths
./target/release/cobol-eval-transpiler

# Or specify custom paths
./target/release/cobol-eval-transpiler \
    --input ./data/HumanEval.jsonl \
    --output ./data/CobolEval.jsonl \
    --verbose
```

## Project Structure

```
COBOLEval/
├── src/                    # Rust transpiler source
│   ├── main.rs            # CLI entry point
│   ├── types.rs           # Type definitions
│   ├── parser.rs          # Python AST parsing
│   ├── transpiler.rs      # COBOL code generation
│   └── constants.rs       # Configuration constants
├── scripts/               # Python evaluation pipeline
│   ├── generate.py        # LLM completion generation
│   ├── evaluation.py      # Correctness checking
│   ├── evaluate_functional_correctness.py  # Pass@k calculation
│   ├── data.py            # Data loading utilities
│   └── utils.py           # Shared utilities
├── data/                  # Datasets
│   ├── HumanEval.jsonl    # Original Python benchmark
│   └── CobolEval.jsonl    # Transpiled COBOL benchmark
├── tests/                 # Python test suite
├── .github/workflows/     # CI/CD configuration
├── pyproject.toml         # Python project configuration
├── Cargo.toml             # Rust project configuration
└── Dockerfile             # Container image definition
```

## Development

### Running Tests

```bash
# Python tests
pytest tests/ -v

# Rust tests
cargo test

# All tests with coverage
pytest tests/ --cov=scripts --cov-report=html
```

### Code Quality

```bash
# Install pre-commit hooks
pip install pre-commit
pre-commit install

# Manual linting
ruff check scripts/
cargo clippy

# Formatting
ruff format scripts/
cargo fmt
```

## Configuration

Environment variables (set in `.env` or export):

| Variable | Description | Default |
|----------|-------------|---------|
| `OPENAI_API_KEY` | OpenAI API key | Required for OpenAI models |
| `COBOLEVAL_TIMEOUT` | Command timeout (seconds) | 30 |
| `HF_TOKEN` | Huggingface token (for gated models) | Optional |
| `RUST_LOG` | Rust logging level | info |

## Benchmark Results

| Model | Pass@1 |
|-------|--------|
| GPT-4 | 10.27% |

*Results from initial benchmarking. See our [blog post](https://bloop.ai/blog/evaluating-llms-on-cobol) for detailed analysis.*

## Citation

If you use COBOLEval in your research, please cite:

```bibtex
@software{coboleval2024,
  author = {bloop.ai},
  title = {COBOLEval: LLM Evaluation for COBOL},
  year = {2024},
  url = {https://github.com/bloopai/COBOLEval}
}
```

## License

MIT License - see [LICENSE](LICENSE) for details.

## Contributing

Contributions are welcome! Please:

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/amazing-feature`)
3. Run tests (`pytest tests/ && cargo test`)
4. Commit changes (`git commit -m 'Add amazing feature'`)
5. Push to branch (`git push origin feature/amazing-feature`)
6. Open a Pull Request

## Acknowledgments

- Based on [HumanEval](https://github.com/openai/human-eval) by OpenAI
- Uses [GnuCOBOL](https://gnucobol.sourceforge.io/) for COBOL compilation
- Developed by [bloop.ai](https://bloop.ai)
