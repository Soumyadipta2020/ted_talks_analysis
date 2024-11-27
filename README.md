# TEDx Talks Analysis 🕵️‍♂️📜

![GitHub Repo stars](https://img.shields.io/github/stars/Soumyadipta2020/ted_talks_analysis?style=social)
![GitHub forks](https://img.shields.io/github/forks/Soumyadipta2020/ted_talks_analysis?style=social)
![GitHub license](https://img.shields.io/github/license/Soumyadipta2020/ted_talks_analysis)
[![HitCount](https://hits.dwyl.com/Soumyadipta2020/ted_talks_analysis.svg?style=flat-square)](http://hits.dwyl.com/Soumyadipta2020/ted_talks_analysis)

Welcome to the **TED Talk Transcript Analysis** project! This repository contains an end-to-end analysis of TED Talk transcripts using **R**, **AI**, and **Quarto** for documentation.

## 🔎 **Project Overview**

This project focuses on analyzing TED Talk transcripts to uncover key insights, such as prevalent topics, opinion strengths, and thematic trends. The analysis leverages **R** for data processing, **AI models** for topic prediction and sentiment analysis, and **Quarto** to create an engaging, interactive report.

## 🚀 **Key Features:**
- **Data Processing**: Cleaning and preprocessing transcript data.
- **Topic Modeling**: Using AI techniques to identify and categorize key themes.
- **Opinion Strength Analysis**: Evaluating how strongly speakers align with various topics.
- **Visualization**: Interactive charts created with **Plotly** and **DT** tables for dynamic data exploration.
- **Quarto Documentation**: Seamlessly integrates code, text, and visuals into an interactive HTML report.

## 🛠️ **Technologies Used:**
- **R**: Data manipulation and analysis.
- **Plotly**: Generating dynamic and interactive visualizations.
- **Quarto**: Creating reproducible and shareable reports.
- **MongoDB**: (Optional) For reading data from a database source.
- **AI Models**: Implemented for topic prediction and sentiment analysis.

## ▶️ **How to Run the Project:**
1. **Clone the Repository:**
   ```bash
   git clone https://github.com/Soumyadipta2020/ted_talks_analysis.git
   cd ted_talks_analysis
   ```

2. **Install Required Packages:**
   ```r
   install.packages(c("dplyr", "DT", "plotly", "jsonlite", "httr2"))
   ```

3. **Render document**

## 📂 **Project Structure:**
- **`data/`**: Contains the sample transcript CSV file.
- **`Analysis.qmd`**: Quarto file for the analysis report.
- **`helper.R`**, **`mongodb_helper.R`**: Custom helper functions.
- **`api.R`** (Not present here) : API key mentioned.
- **`README.md`**: Project documentation.
- **`manifest.json`**: Environment captured.

## 💡 **Contribution:**

Contributions are welcome! If you have ideas to enhance the app or fix issues, feel free to fork the repository, make changes, and submit a pull request.

Steps to Contribute:

1. Fork this repository.
2. Create a new branch: `git checkout -b feature-name`
3. Commit your changes: `git commit -m "Add feature-name"`
4. Push to your branch: `git push origin feature-name`
5. Open a Pull Request.

---

Happy Analyzing! 🎤📊
