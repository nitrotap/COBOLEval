# COBOLEval Docker Image
# Provides a complete environment for running COBOLEval benchmarks

FROM rust:1.75-bookworm AS rust-builder

WORKDIR /app
COPY Cargo.toml Cargo.lock* ./
COPY src/ ./src/

RUN cargo build --release

# Main image
FROM python:3.11-slim-bookworm

# Install system dependencies including GnuCOBOL
RUN apt-get update && apt-get install -y --no-install-recommends \
    gnucobol \
    gcc \
    libcob4 \
    && rm -rf /var/lib/apt/lists/*

# Verify GnuCOBOL installation
RUN cobc --version

WORKDIR /app

# Copy Python project files
COPY pyproject.toml README.md LICENSE ./
COPY scripts/ ./scripts/
COPY data/ ./data/
COPY tests/ ./tests/

# Copy Rust binary from builder
COPY --from=rust-builder /app/target/release/cobol-eval-transpiler /usr/local/bin/

# Install Python dependencies
RUN pip install --no-cache-dir -e ".[dev]"

# Create non-root user for security
RUN useradd -m -u 1000 coboleval
RUN chown -R coboleval:coboleval /app
USER coboleval

# Default working directory for evaluations
WORKDIR /app

# Default command shows help
CMD ["python", "-m", "scripts.generate", "--help"]

# Labels
LABEL org.opencontainers.image.title="COBOLEval"
LABEL org.opencontainers.image.description="LLM Evaluation benchmark for COBOL code generation"
LABEL org.opencontainers.image.source="https://github.com/bloopai/COBOLEval"
LABEL org.opencontainers.image.licenses="MIT"
