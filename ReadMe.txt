
The purpose of this document is to explain what each of the 
scripts in this repository is for. What package or papers is 
it referencing, what is the purpose of the code. 

#===========================#
# Event_studies_and_friends #
#===========================#

#===============================================#
# sources for Event studies and Friends package #
#===============================================#
This is just a word document with some notes and sources. 
Mostly just for me (nate Mather) to have some internal notes about different packages, papers, and links 





#=============================#
# Investigate Packages Folder #
#=============================#


	#=========================#
	# Abraham and Sun Methods #
	#=========================#
	
	This is essentially just a replication of "Estimating Dynamic Treatment Effects in Event Studies
	with Heterogeneous Treatment Effects" by Abraham and Sun. This code is just meant to get this method replicated 
	in R and to give me/us a better understanding of it. This will be the foundation for writing an actual function 
	to implement their method. 


	#========================#
	# understanding Augsynth #
	#========================# 

	This is mostly just the code from the vignette for the augsynth package. Having it in this repository 
	as actual code will just help me in the future if we are working it into a large wrapper function. This
	Is of course from the “The Augmented Synthetic Control Method” paper by Eli Ben-Michael, Avi Feller, 
	and Jesse Rothstein. 

	#===================#
	# Understanding WFE #
	#===================#

	Get working exampels of the main funcitons in the "wfe" package 
	this is all pulled from the replication files, which can be downloaded 
	here: https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/YUM3K8 
 
	These files are from their corresponding paper  “When Should We Use Unit Fixed Effects 
	Regression Models for Causal Inference with Longitudinal Data?” by Kosuke Imai and In Song Kim.

	Paper can be found  here: https://imai.fas.harvard.edu/research/files/FEmatch.pdf  

	the advatage of this over just looking at their code is it is stripped down
   	and has extra comments for clarity. This is specifically to show working examples
   	of the funcitons rather than code creating outpput for a paper

	#==========================#
	# Understanding PanelMatch #
	#==========================#

	Get working examples of functions from the PanelMatch package 
 	put out by Imai, Kosuke. It implimets methods from the paper 
	'Matching Methods for Causal Inference with Time-Series Cross-Sectional Data" by 
	Kosuke Imai, In Song Kim, Erik Wang. Which can be found here: https://imai.fas.harvard.edu/research/files/tscs.pdf

	#===================#
	# Understanding did #
	#===================#

	Get working examples of functions from the  DID package 
	package for relavent working paper 
	Callaway, Brantly and Sant'Anna, Pedro. "Difference-in-Differences with Multiple Time Periods." 
	Working Paper https://ssrn.com/abstract=3148250, 2019 

	#===================#
	# Understanding PLM #
	#===================#

	If we use this at all it would be to impliment methods where we need to run fixed effects models. The main
	use is just the  plm function which is for "Linear models for panel data estimated using the lm function on transformed data."
	I am increasingly thinking the LFE package is better in general than the PLM packge though so this may not be useful. 
 

#==================#
# Functions Folder #
#==================#

This is the folder for new functions that we create 

	#======================#
	# Abraham_sun_function #
	#======================#

	Putting the Abraham and Sun methods from Estimating Dynamic Treatment Effects in Event Studies
	with Heterogeneous Treatment Effects" into a generalizable function. This will estimate their IW estimates.

	

	#=========================================#
	# ==== Bilinski and Hatfield Function ====
	#=========================================#

	use the method from the bilinski and Hatfield function to impliment their test on a pretrends test
	there extra stuff around the function for debugging. Specifically I make some simulated data in here.
	we can move that to a seperate testing script once we are closer to a final version 

#=======================#
# function tests folder #
#=======================#

Just what it sounds like. runs tests on the functions in the functions folder 



	#================================#
	# ==== abraham and sun tests ====
	#================================#

