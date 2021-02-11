# Gender Differences in Risk Preference on Jeopardy
This repository examines wagering decisions on the game show Jeopardy to compare risk preference differences between men and women. The final PDF of the thesis can be found in the folder "Final Thesis" under the name "thesis_itself."

## Scraping the J!Archive
The first part of my thesis is scraping www.j-archive.com, an online repository of every Jeopardy game in history.

## Data Set Manipulation
Next, I manipulate the scraped data to be amenable for different types of data analyses in my thesis. The archive does not include gender, so I use a package to assign each given name a probability of being male.

## Analyses
Finally, I run the analyses. This starts with a simple regression of wager on gender. Then, I add controls. Finally, I introduce more complicated utility models and estimate risk-parameters for each gender.

## Final Thesis
The results are compiled in an MD file that is knitted to a PDF.
