# ResearchProject_Template

Driving questions for this project included determining whether Guadua foliage leaf micromorphology differs depending on factors including 
habitat, habit, and region in which specimens were collected

The a compilation of all code on tidy data (data analysis and graphics) is found in McMurchieProject/590A_assignments/Own_data_analysis_EM_in_progress. 

Analysis folder has data analysis code, graphics folder has correlation table and PCoA code

Major issues remaining:
- Need to figure out how to rename rows/columns of correlation table
- Broom, gt, and gt summary are NOT compatible with RRPP. Need to find a package to make pretty tables that is RRPP compatible
	- RRPP is not made with tidyverse in mind
- Need more specimens so we can run post-hoc analyses
	- This would also allow us to use all three categories for habit and habitat for analysis
- Likely need to reconsider habit factor. Currently ALL savanna specimens are "small arching" habit, resulting in non-full matrix