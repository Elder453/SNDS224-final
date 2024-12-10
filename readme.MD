# **S&DS 224: Pig Game Strategy Analysis**
## Author: Elder Veliz

This repository contains all the files and resources used for the strategic analysis and optimization of the Pig game. The project leverages probabilistic modeling, heuristic strategies, and Markov Decision Processes (MDPs) to derive optimal strategies and compare them against heuristic approaches.

---

## **Repository Structure**

- **`env.yml`**  
  - Contains the required R libraries for running the project. Use this file to set up the environment by creating a compatible R environment.

- **`src/`**  
  - Contains the source R script files for each function used in the project. This includes code for simulations, value iteration, policy extraction, and data visualization. It also include the Quarto Markdown source file for the report and the PowerPoint slides for the presentation.

- **`data/`**  
  - Contains pre-saved versions of the simulation results to avoid re-running time-intensive computations. These include results for heuristic strategies, optimal policy training, and match-up simulations.

- **`deliverables/`**  
  - Includes the final presentation and report in PDF format.

---

## **Setup Instructions**

1. **Clone the Repository**:

2. **Set Up Environment**:
   Ensure you have R and `conda` installed. Then create the required environment using the `env.yml` file:
   ```bash
   conda env create -f env.yml
   conda activate pig-game-env
   ```

3. **Run Scripts**:
   - Navigate to the `src/` folder and execute the R scripts to replicate simulations and visualizations.
   - If you'd like to avoid re-running simulations, use the pre-saved data in the `data/` folder.
   - To explicitly re-run the simulations, please modify the `deliverables/report.qmd` file, changing the `SIMULATE` variable to `TRUE`.

4. **Review Deliverables**:
   - Review the **report** and **presentation** in the `deliverables/` folder for detailed analysis and insights.

---

## **Key Highlights**

- **Simulations**: Evaluates both heuristic and optimal strategies under various conditions.
- **Optimal Policy**: Derived using value iteration, tested against the heuristic strategy, and visualized using heatmaps.
- **Starting Player Advantage**: Quantifies the inherent advantage of playing first and explores strategies to mitigate it.

---

## **Future Improvements**

- Analyze the influence of target score changes.
- Explore randomness in decision-making for realistic human behavior.
- Investigate methods to balance the starting player advantage.

---

For any questions or further clarification, feel free to contact the author, [Elder Veliz](mailto:elder.veliz@yale.edu)!