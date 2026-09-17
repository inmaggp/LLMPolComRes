# LLMPolComRes, Large Language Models vs. Human Coders in Political Communication Research

This repository contains all code, data, and documentation required to reproduce the analyses reported in the manuscript *“Large Language Models vs. Human Coders in Political Communication Research”*.  
The project is organized around a single fully reproducible R Markdown workflow, complemented by three standalone R scripts and one Jupyter notebook for users who prefer modular execution.

---

## 1. Repository Structure

The repository includes the following files, as described in the manuscript and in the reproducibility report:

```
LLMPolComRes/
│
├── Markdown.Rmd
├── Markdown.html
│
├── Markdown_PilotDataset.R
├── Markdown_MainProject.R
├── Markdown_MainProject_LLaMa.R
│
├── Perform_LLaMa.ipynb
│
├── Data/
│   ├── Pilot Data.xlsx
│   ├── Main Data.xlsx
│   ├── Pilot_classification.xlsx
│   ├── Main_classification.xlsx
│   ├── pilot_data_1_200.xlsx
│   ├── main_data_1_200.xlsx
│   ├── binarizations_LLaMa.xlsx
│   ├── binarizations_LLM.xlsx
│   ├── main_binarizations_LLaMa.xlsx
│   ├── main_binarizations_BART.xlsx
│
└── Results/
    ├── Pilot/
    └── Main Project/
```

---

## 2. Reproducibility Workflow

### Recommended: Full workflow (Markdown.Rmd)

The file **Markdown.Rmd** provides a complete, step‑by‑step analytical workflow including:

- the purpose of each experiment  
- the execution order  
- the commands required to reproduce all results  
- explanations of each analytical step  
- links to intermediate outputs  

It can be knitted directly:

```r
rmarkdown::render("Markdown.Rmd")
```

This is the **preferred reproducible path**.

---

## 3. Individual Scripts (Modular Execution)

For users who prefer to reproduce each analysis separately, the repository includes three R scripts and one notebook:

### Markdown_PilotDataset.R  
Reproduces the pilot dataset analysis.  
Includes preprocessing, zero-shot classification (ZSC), binarization, metric computation, and figure generation.  
The ZSC step can be skipped using:

```bash
SKIP_ZSC=YES
```
### Markdown_MainProject.R  
Reproduces the BART analysis of the main dataset.  
Includes preprocessing, zero-shot classification (ZSC), binarization, metric computation, and figure generation.  
The ZSC step can be skipped using:

```bash
SKIP_ZSC=YES
```

### Markdown_MainProject_LLaMa.R  
Reproduces the LLaMA analysis of the main dataset using the classifications generated in the notebook.

### Perform_LLaMa.ipynb  
Performs LLaMA classification for both pilot and main datasets.  
Requires access approval from Meta:  
`https://huggingface.co/meta-llama/Llama-3.1-8B-Instruct`

Runtime used in the reproducibility check:  
**Google Colab 2026.07 — GPU T4**

Running this notebook in separate batches for the different sets of texts produces the LLaMA classifications stored in the **LLaMA** sheet of:

- `Data/Pilot_classification.xlsx`  
- `Data/Main_classification.xlsx`

---

## 4. Zero‑Shot Classification (ZSC) Function

The **ZSC function** is the core procedure used to obtain classifications from BART (or any other Hugging Face LLM).  
It operates by iteratively evaluating **each text–hypothesis–issue combination** and returning probability scores.

### Key points:
- The function is computationally intensive: running the full dataset in one pass is **not feasible**.  
- It should be executed in **smaller batches** of texts.  
- The outputs are stored in Excel files:
  - **LLM sheet** → contains classifications from BART (or other Hugging Face LLMs).  
  - **LLaMA sheet** → contains classifications from LLaMA (generated via the notebook).  

This design ensures that both pilot and main datasets can be reproduced without requiring extreme computational resources.

---

## 5. Computational Requirements

Zero-shot classification is computationally expensive.  
Running the full classification for the complete datasets is **not feasible on CPU**.

### Recommendations
- Use **GPU** (e.g., Colab T4).  
- Process texts in **small batches**.  
- Use the provided **200‑text test datasets** for quick checks:
  - `pilot_data_1_200.xlsx`
  - `main_data_1_200.xlsx`

---

## 6. Docker Environment (Reviewer’s Setup)

The reviewer provided a Docker environment for full reproducibility:

```bash
docker compose build
docker compose run --remove-orphans --rm tbv Rscript Functions.R
```

For Quarto rendering:

```bash
docker compose run --remove-orphans --rm tbv quarto render Markdown_MainProject_LLaMa.R
```

---

## 7. Origin of Classification Files

### Data/Main_classification.xlsx  
Contains:
- BART classifications generated via `Markdown_MainProject.R` (stored in the **BART** sheet)  
- LLaMA classifications generated via `Perform_LLaMa.ipynb` (stored in the **LLaMA** sheet)

### Data/Pilot_classification.xlsx  
Contains:
- BART classifications generated via `Markdown_PilotDataset.R` (stored in the **LLM** sheet)  
- LLaMA classifications generated via `Perform_LLaMa.ipynb` (stored in the **LLaMA** sheet)

Both files are produced by combining batch outputs, as running the full dataset in one pass is not feasible.

---

## 8. Summary of Tables and Figures

### Main Study

**Table 1 — Fleiss’ Kappa**  
- BART: `Results/Main Project/BART/results_main_BART.xlsx`  
- LLaMA: `Results/Main Project/LLaMA/results_main_LLaMA.xlsx`

**Table 2 — Classification Metrics**  
- Same files as Table 1

**Figure 1 — Alignment of BART with Humans**  
- `Results/Main Project/BART/New_BarsPoints4_main_BART.png`  
- Generated in `Markdown_MainProject.R`, lines 357–412

**Figure 2 — Alignment of LLaMA with Humans**  
- `Results/Main Project/LLaMa/New_BarsPoints4_main_LLaMa.png`  
- Generated in `Markdown_MainProject_LLaMa.R`, lines 247–304

---

### Appendix B

**Table A1 — Selected Pilot Posts**  
- From `Data/Pilot_classification.xlsx` (highlighted rows)

**Table A2 — Pearson Correlations**  
- Generated in `Markdown_PilotDataset.R`, lines 484–516

**Figures 1–3 — Pilot Study Analyses**  
- Generated in `Markdown_PilotDataset.R`  
- Located in `Results/Pilot/`

---

## 9. Contact

For questions or issues, please open an issue in the repository or contact the authors.
```
