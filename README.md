# UGA Covid-19 Tracker

This repository contains the files underlying [this COVID-19 tracker.](https://shiny.ovpr.uga.edu/yact/) 
See the **About** tab on the tracker website for more information.

A cronjob is deployed which runs everyday night and produces the new dataset, deletes the old ones and restarts the server. The cronjob looks something like:

01 00 * * * cd <path to the folder> && Rscript <path to the folder>/data_cron.R && bash <path to the folder>/delete.sh && sudo systemctl restart shiny-server.service