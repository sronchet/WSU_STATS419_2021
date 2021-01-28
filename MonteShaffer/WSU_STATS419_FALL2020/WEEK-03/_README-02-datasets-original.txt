Title:  Assignment "Datasets" (15 points)

Due:  Please submit on Blackboard before Tuesday September 8 at 11:59PM (before midnight).

Deliverable:  Prepare a document (preferably a PDF) that includes at the top right:  Your name, WSU email, WSU Cougar ID Number, STATS 419, Instructor: Monte J. Shaffer, Date

Instructions:  For each question, include the original question and your response below the original question.


-------------


1.	Create the "rotate matrix" functions as described in lectures.  Apply to the example "myMatrix".

				myMatrix = matrix ( c (
											1, 0, 2,
											0, 3, 0,
											4, 0, 5
											), nrow=3, byrow=T);

				transposeMatrix = function(mat)
					{
					t(mat);	
					}
					
				#rotateMatrix90(mat)	
				#rotateMatrix180(mat)	
				#rotateMatrix270(mat)
				# 3x3 matrix ... ## matrix multiplication


2.	Recreate the graphic for the IRIS Data Set using R.  Same titles, same scales, same colors.  See: https://en.wikipedia.org/wiki/Iris_flower_data_set#/media/File:Iris_dataset_scatterplot.svg

3.  Right 2-3 sentences concisely defining the IRIS Data Set.  Maybe search KAGGLE for a nice template.  Be certain the final writeup are your own sentences (make certain you modify what you find, make it your own, but also cite where you got your ideas from).  NOTE:  Watch the video, Figure 8 has a +5 EASTER EGG.

4.  Import "personality-raw.txt" into R.  Remove the V00 column.  Create two new columns from the current column "date_test":  year and week. Stack Overflow may help:  https://stackoverflow.com/questions/22439540/how-to-get-week-numbers-from-dates ... Sort the new data frame by YEAR, WEEK so the newest tests are first ... The newest tests (e.g., 2020 or 2019) are at the top of the data frame.  Then remove duplicates using the unique function based on the column "md5_email".  Save the data frame in the same "pipe-delimited format" ( | is a pipe ) with the headers.  You will keep the new data frame as "personality-clean.txt" for future work (you will not upload it at this time).  In the homework, for this tasks, report how many records your raw dataset had and how many records your clean dataset has.

5.  Write functions for doSummary and sampleVariance and doMode ... test these functions in your homework on the "monte.shaffer@gmail.com" record from the clean dataset.  Report your findings.  For this "monte.shaffer@gmail.com" record, also create z-scores.  Plot(x,y) where x is the raw scores for "monte.shaffer@gmail.com" and y is the z-scores from those raw scores.  Include the plot in your assignment, and write 2 sentences describing what pattern you are seeing and why this pattern is present.

6.  Compare Will Smith and Denzel Washington. [See 03_n greater 1-v2.txt for the necessary functions and will-vs-denzel.txt for some sample code and in DROPBOX: \__student_access__\unit_01_exploratory_data_analysis\week_02\imdb-example ]  You will have to create a new variable $millions.2000 that converts each movie's $millions based on the $year of the movie, so all dollars are in the same time frame.  You will need inflation data from about 1980-2020 to make this work.

7.  Build side-by-side box plots on several of the variables (including #6) to compare the two movie stars.  After each box plot, write 2+ sentence describing what you are seeing, and what conclusions you can logically make.  You will need to review what the box plot is showing with the box portion, the divider in the box, and the whiskers.


Rubric (see syllabus for [F] and [S] criteria):
[C] Content (Responses): 9
[F] Adherence to Format Requirements: 3
[S] Style: 3


