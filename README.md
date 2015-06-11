org-issue-sync
==============

Synchronizes org-mode files with various sources of issues or bug trackers.

Currently working
-----------------

* Downloads issues from Github and Google Code and Google Tasks
* Identifies issues previously downloaded in one of many .org files
* Appends new issues into a specified destination .org file
* Update issues in .org files that have changed
* Download more information (description, events) about each issue
* OAuth Tokens

Planned
-------
* Separate the org-mode I/O core from the issues system
* Re-arch the config file system to support reload
* Let this importer work as a recurring automatic service, either autolaunched or 
