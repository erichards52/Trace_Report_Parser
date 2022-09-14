# Trace_Report_Parser
Parses large number of trace reports from Nextflow to provide meaningful output.
Need to add your own process(es) before using this script.
Returns averages and nothing more, but provides vectors for each process pulled out (more could theoretically be added if those are of interest).

## How to use (for Trace_Parser.R) 
Place your trace reports that you have SCP'd from the cluster where you will pull them from into your RStudio (i.e.: mine are located in `/Users/edwardrichards/Documents/All_trace/` as can be seen here):
`filenames_dirs <- list.dirs("/Users/edwardrichards/Documents/All_trace/",full.names=TRUE)`

Rename "PROCESS" found on this line to the process you wish to investigate (i.e.: if you open the trace reports, there should be individual process names like "MINIMAP_2_BAM":

`ldfProcess <- lapply(ldf, subset, grepl("PROCESS", process))` change this to 
`ldfProcess <- lapply(ldf, subset, grepl("MINIMAP_2_BAM", process))`

From here, you can run the rest of the script, with it returning duration (total runtime + waittime), realtime (total runtime), %CPU (percentage CPU used), VMEM (total memory used), RCHAR (read characters), WCHAR (written characters) for the process specified - it will print the results as you work through the script. 

You will have to create and feed these results into your own table.

