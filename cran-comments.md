This is a resubmission of the package 'interpretCI'.

## Test environments
* local OS X install, R 4.1.2
* win-builder (devel and release)
* rhub

## R CMD check results
There were no ERRORs or WARNINGs.

## Comment from CRAN on the last submission and my reply

Please add \value to .Rd files regarding exported methods and explain the functions results in the documentation. Please write about the structure of the output (class) and also what the output means. (If a function does not return a value, please document that too, e.g. \value{No return value, called for side effects} or similar)
Missing Rd-tags:
     interpret.Rd: \value
     is.mynumeric.Rd: \value
     meanCI_sub.Rd: \value
     meanCI2.Rd: \value
     plot.meanCI.Rd: \value
     print.meanCI.Rd: \value
     print.plotCI.Rd: \value
     propCI_sub.Rd: \value
     propCI.Rd: \value
     rstudio_viewer.Rd: \value
     
-> I have added \value to all .Rd files. Thank you ! 

\dontrun{} should only be used if the example really cannot be executed (e.g. because of missing additional software, missing API keys, ...) by the user. That's why wrapping examples in \dontrun{} adds the comment ('# Not run:') as a warning for the user.
Does not seem necessary.

Please unwrap the examples if they are executable in < 5 sec, or replace \dontrun{} with \donttest{}.
-> I have replaced \dontrun{} with \donttest{} in plot.meanCI() function because they are not executable in < 5 sec 
-> But the examples in interprete() function reallly cannot be executed so I have to use \donttrun{} (Because this function makes a html file and shows it in RStudio viewer)

Please fix and resubmit.
-> I have fixed all you commented. Thank you !
