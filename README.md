## scraping crowdfunding pages (R scripts)

R scripts to study and analyze (civic) crowdfunding projects

This research project studies civic crowdfunding projects using web scraping.
This is an experimental repo. Please contact us if you would like to contribute to this project or make use of the code.

Flow:
* hand-picked selection of platforms (feel free to add) - Only ENG & NL are included in the analysis at the moment
* extract project links from platforms
* extract variables from projects: successful, pledged amount, backer count, Facebook fans, pitch, video present, comment count)

Files:
* platforms.csv - overview of platforms - Feel free to add
* projects.csv - overview of scraped projecturls (platform, url, timestamp)
* projectpages.R - R script to extract project urls from platform pages (output: projects.scv)
* projectscraping.R - R script to extract variables from project pages (output: dataset.csv)

More information? Contact bastiaan.baccarne@ugent.be
