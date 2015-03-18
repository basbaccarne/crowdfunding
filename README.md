## scraping crowdfunding pages (R scripts)

R scripts to study and analyze (civic) crowdfunding projects

This research project studies civic crowdfunding projects using web scraping.
This is an experimental repo. Please contact us if you would like to contribute to this project or make use of the code.

Flow:
* hand-picked selection of platforms (feel free to add) - Only ENG & NL are included in the analysis at the moment
* extract project links from platforms
* extract variables from projects: successful, pledged amount, backer count, Facebook fans, pitch, video present, comment count)

Files:
* platforms.csv - overview of platforms (names, url, projecturl, language) - Feel free to add
* projects.csv - overview of de projecturls (platform, url, timestamp) on different platforms
* projectpages.R - R script to extract project urls from platform pages (output: projects.scv)
  * platforms ok: voorjebuurt, citizinvestor, growfunding, IOBY, geeferom, hkb, zcfp, dordrechtvanstart, maakcapelle, communityfunded, uruut, smallknot
  * platforms not fully operational: neighbour.ly, spacehive, onepercentclub
  * platforms temp excluded (lang): urbankit, goteo, ideaginger
  * 2 DO: clean code (concise and better flexibity)
* projectscraping.R - R script to extract variables from project pages (output: dataset.csv)
  * status: to be started

More information? Contact bastiaan.baccarne@ugent.be