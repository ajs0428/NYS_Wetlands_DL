# Model Architecture Diagrams

Mermaid diagrams for the U-Net and Dual-Branch U-Net architectures.
Open this file in any Mermaid-compatible viewer (GitHub, VS Code with Mermaid extension, mermaid.live, etc.)

---

## U-Net with Residual Blocks + SE Attention

```mermaid
flowchart TB
    subgraph INPUT["Input"]
        A["18-band Raster Patch<br/>(B, 18, 256, 256)<br/>Terrain + Spectral + SAR + NAIP"]
    end

    subgraph ENCODER["Encoder (Residual ConvBlocks + MaxPool)"]
        direction TB
        E1["Encoder 1<br/>18 → 32 filters<br/>256×256"]
        E2["Encoder 2<br/>32 → 64 filters<br/>128×128"]
        E3["Encoder 3<br/>64 → 128 filters<br/>64×64"]
        E4["Encoder 4<br/>128 → 256 filters<br/>32×32"]
    end

    subgraph BOTTLENECK["Bottleneck"]
        BN["ConvBlock<br/>256 → 512 filters<br/>16×16<br/>+ Spatial Dropout"]
    end

    subgraph DECODER["Decoder (ConvTranspose + SE Attention)"]
        direction TB
        D4["Decoder 4<br/>512 → 256 filters<br/>32×32"]
        D3["Decoder 3<br/>256 → 128 filters<br/>64×64"]
        D2["Decoder 2<br/>128 → 64 filters<br/>128×128"]
        D1["Decoder 1<br/>64 → 32 filters<br/>256×256"]
    end

    subgraph OUTPUT["Output"]
        OUT["1×1 Conv<br/>32 → 4 classes<br/>(B, 4, 256, 256)<br/>EMW | FSW | SSW | UPL"]
    end

    A --> E1
    E1 --> E2
    E2 --> E3
    E3 --> E4
    E4 --> BN

    BN --> D4
    D4 --> D3
    D3 --> D2
    D2 --> D1
    D1 --> OUT

    E4 -. "skip connection" .-> D4
    E3 -. "skip connection" .-> D3
    E2 -. "skip connection" .-> D2
    E1 -. "skip connection" .-> D1

    style INPUT fill:#e8f4f8,stroke:#2980b9,stroke-width:2px
    style ENCODER fill:#fef9e7,stroke:#f39c12,stroke-width:2px
    style BOTTLENECK fill:#fdedec,stroke:#e74c3c,stroke-width:2px
    style DECODER fill:#eafaf1,stroke:#27ae60,stroke-width:2px
    style OUTPUT fill:#f4ecf7,stroke:#8e44ad,stroke-width:2px
```

### Encoder Block Detail

```mermaid
flowchart LR
    subgraph EncoderBlock["Encoder Block"]
        direction LR
        C1["Conv 3×3"] --> BN1["BatchNorm"] --> R1["ReLU"]
        R1 --> C2["Conv 3×3"] --> BN2["BatchNorm"]
        BN2 --> ADD["Add ⊕"]
        C1 -. "1×1 shortcut<br/>(residual)" .-> ADD
        ADD --> R2["ReLU"] --> MP["MaxPool 2×2<br/>↓2"]
    end

    style EncoderBlock fill:#fef9e7,stroke:#f39c12,stroke-width:2px
```

### Decoder Block Detail

```mermaid
flowchart LR
    subgraph DecoderBlock["Decoder Block"]
        direction LR
        UP["ConvTranspose<br/>↑2"] --> CAT["Concat<br/>+ Skip"]
        CAT --> C1["Conv 3×3"] --> BN1["BatchNorm"] --> R1["ReLU"]
        R1 --> C2["Conv 3×3"] --> BN2["BatchNorm"] --> R2["ReLU"]
        R2 --> SE["SE Attention<br/>AvgPool → FC → σ"]
    end

    style DecoderBlock fill:#eafaf1,stroke:#27ae60,stroke-width:2px
```

---

## Dual-Branch U-Net (ResNet-34 + ResNet-18 with Gated Fusion)

```mermaid
flowchart TB
    subgraph INPUT["Input: 18-band Raster Patch (B, 18, 256, 256)"]
        direction LR
        SPLIT["Channel Split"]
    end

    subgraph OPTICAL_BRANCH["Optical Branch — ResNet-34"]
        direction TB
        O0["Stem: 11ch → 64<br/>128×128"]
        O1["Stage 1: 64 → 64<br/>3 blocks, 128×128"]
        O2["Stage 2: 64 → 128<br/>4 blocks, 64×64"]
        O3["Stage 3: 128 → 256<br/>6 blocks, 32×32"]
        O4["Stage 4: 256 → 512<br/>3 blocks, 16×16"]
    end

    subgraph TERRAIN_BRANCH["Terrain Branch — ResNet-18"]
        direction TB
        T0["Stem: 8ch → 64<br/>128×128"]
        T1["Stage 1: 64 → 64<br/>2 blocks, 128×128"]
        T2["Stage 2: 64 → 128<br/>2 blocks, 64×64"]
        T3["Stage 3: 128 → 256<br/>2 blocks, 32×32"]
        T4["Stage 4: 256 → 512<br/>2 blocks, 16×16"]
    end

    subgraph BAND_LABELS["Band Assignment"]
        direction TB
        OPT_BANDS["Optical (11 bands)<br/>EVI, NDYI, GDVI<br/>VV, VH<br/>r, g, b, nir<br/>n_ndvi, n_ndwi"]
        TER_BANDS["Terrain (8 bands)<br/>DEM, meanc, planc<br/>profc, dmv, slope<br/>TPI, CHM"]
    end

    subgraph FUSION["Gated Fusion (per stage)"]
        direction TB
        F0["Fuse Stage 0: 64ch"]
        F1["Fuse Stage 1: 64ch"]
        F2["Fuse Stage 2: 128ch"]
        F3["Fuse Stage 3: 256ch"]
        F4["Fuse Stage 4: 512ch"]
    end

    subgraph DECODER["Shared Decoder + SE Attention"]
        direction TB
        D4["Decoder 4: 512 → 256<br/>32×32"]
        D3["Decoder 3: 256 → 128<br/>64×64"]
        D2["Decoder 2: 128 → 64<br/>128×128"]
        D1["Decoder 1: 64 → 32<br/>256×256"]
    end

    subgraph OUTPUT_HEAD["Output"]
        FINAL_UP["ConvTranspose ↑2<br/>32 → 32, 256×256"]
        OUT["1×1 Conv → 4 classes<br/>(B, 4, 256, 256)<br/>EMW | FSW | SSW | UPL"]
    end

    SPLIT --> O0
    SPLIT --> T0
    OPT_BANDS -.- O0
    TER_BANDS -.- T0

    O0 --> O1 --> O2 --> O3 --> O4
    T0 --> T1 --> T2 --> T3 --> T4

    O0 & T0 --> F0
    O1 & T1 --> F1
    O2 & T2 --> F2
    O3 & T3 --> F3
    O4 & T4 --> F4

    F4 --> D4
    F3 -. "skip" .-> D4
    D4 --> D3
    F2 -. "skip" .-> D3
    D3 --> D2
    F1 -. "skip" .-> D2
    D2 --> D1
    F0 -. "skip" .-> D1

    D1 --> FINAL_UP --> OUT

    style INPUT fill:#e8f4f8,stroke:#2980b9,stroke-width:2px
    style OPTICAL_BRANCH fill:#fef9e7,stroke:#f39c12,stroke-width:2px
    style TERRAIN_BRANCH fill:#d5f5e3,stroke:#27ae60,stroke-width:2px
    style BAND_LABELS fill:#fafafa,stroke:#bbb,stroke-width:1px,stroke-dasharray:5
    style FUSION fill:#fdedec,stroke:#e74c3c,stroke-width:2px
    style DECODER fill:#ebf5fb,stroke:#3498db,stroke-width:2px
    style OUTPUT_HEAD fill:#f4ecf7,stroke:#8e44ad,stroke-width:2px
```

### Gated Fusion Detail

```mermaid
flowchart LR
    subgraph GatedFusion["Gated Fusion Module"]
        direction LR
        OPT["Optical<br/>Features"] --> CAT["Concat"]
        TER["Terrain<br/>Features"] --> CAT
        CAT --> GAP["Global<br/>AvgPool"]
        GAP --> FC1["FC → ReLU"]
        FC1 --> FC2["FC → Sigmoid"]
        FC2 --> GATE["Gate (g)"]
        GATE --> MIX["optical × g +<br/>terrain × (1-g)"]
        OPT --> MIX
        TER --> MIX
    end

    style GatedFusion fill:#fdedec,stroke:#e74c3c,stroke-width:2px
```

---

## Rendering Instructions

1. **GitHub**: Push this file — GitHub renders Mermaid natively
2. **VS Code**: Install "Markdown Preview Mermaid Support" extension
3. **Web**: Paste diagrams at [mermaid.live](https://mermaid.live) — export as SVG/PNG
4. **Slides**: Export as SVG from mermaid.live, then insert into PowerPoint/Google Slides
