# Base image: PyTorch with CUDA 11.8 — compatible with all GPU nodes
# (A40 11.1+, A100 11.0+, H100 11.8+, A6000 12.6)
FROM pytorch/pytorch:2.4.1-cuda11.8-cudnn9-runtime

ENV PYTHONUNBUFFERED=1
ENV DEBIAN_FRONTEND=noninteractive

# System deps for rasterio/GDAL
RUN apt-get update && apt-get install -y --no-install-recommends \
    gdal-bin libgdal-dev g++ \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app

# Install Python deps from pyproject.toml (torch/torchvision already in base image)
COPY pyproject.toml .
RUN pip install --no-cache-dir \
    $(python -c "import tomllib; \
      deps=tomllib.load(open('pyproject.toml','rb'))['project']['dependencies']; \
      print(' '.join(d for d in deps if not d.startswith(('torch',))))" \
    ) jupyterlab ipykernel

# Copy pipeline code
COPY Python_Code_Analysis/ Python_Code_Analysis/

# Expose ports for Jupyter and TensorBoard (BioHPC allows 8009-8039)
EXPOSE 8015 8016

# Data/ and Models/ are mounted at runtime, not baked in
# e.g.: docker1 run --gpus all -v /workdir/<labid>/Data:/app/Data ...

CMD ["python", "Python_Code_Analysis/DL_Pipeline_v2/dl_04_train_lightning.py"]
