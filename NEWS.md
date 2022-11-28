# PlotR app development version

## PlotR 22.11.1

### New Features
- button to export the plot in "Run Model" tab (#9) and in "Post Processing" tab

### Updates
- in the "Run Model" tab:
  - change the default value of "Select a file" to the last uploaded file (not the first upload) (#6)
  - add a load button to load a selected file (#11)
- update the UI:
  - remove right sidebar, 
  - integrate export buttons into main panel,
  - in "Run model tab" load plot inputs directly above the plot view

### Bug Fixes
- reset column selection and model parameters after selecting a new file (#6)
- transform columns to numeric columns when running a model (#8), all NA's are removed automatically
- fix wrong default values for width and height when exporting a plot (#9, #10)
- fix `tryCatch` when fitting a model, error message is now forwarded correctly to a pop up (#9)
- fix import/upload of files with the same name (#11)
