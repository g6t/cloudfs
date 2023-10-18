## Resubmission
This is a resubmission. Below are my responses to the feedback received in the
previous review.

### User options

```
Please always make sure to reset to user's options(), working directory or par() after you changed it in examples and vignettes and demos. --> inst/doc/cloudfs.R
e.g.:
old <- options(digits = 3)
...
options(old)
```

I've addressed this by removing the `options(width = 150)` command from the
beginning of the cloudfs.Rmd vignette.

### User Space Integrity

```
Please ensure that your functions do not write by default or in your
examples/vignettes/tests in the user's home filespace (including the package
directory and getwd()). This is not allowed by CRAN policies.
Please omit any default path in writing functions. In your
examples/vignettes/tests you can write to tempdir().
e.g.: man/cloud_drive_spreadsheet_autofit.Rd ; man/cloud_drive_upload.Rd ; ...
```

I understand the importance of adhering to CRAN policies and ensuring that there
are no unintended consequences for the users of the package. However, I have
chosen not to make the suggested modifications, and I'd like to explain the
rationale behind this decision and why in my opinion the package does not
violate the policies.

One of the key features of the package is that it enables the use of concise
relative paths for both the current working directory and associated cloud
project folders. For instance, consider the task of uploading a local file,
"models/glm.rds", to a project's S3 folder. Using `aws.s3`, the code would be:

```R
aws.s3::put_object(
  file = "models/glm.rds",
  bucket = "project-data",
  object = "project-1/models/glm.rds"
)
```

With `cloudfs`, it can be achieved with a significantly simpler syntax:

```R
cloud_s3_upload("models/glm.rds")
```

Applying `cloud_s3_upload()` to a file located in a temporary folder goes
against its design intent. Its main objective is to upload files while mirroring
the folder structure between the current directory and the project's S3.
Demonstrating this with a temp folder file would misrepresent the function's
typical application.

That being said I've taken comprehensive measures to ensure no accidental or
default file writing occurs in the current working directory:

- **Initial Setup**: Most functions, including all `*read*`, `*write*`,
`*upload*`, `*download*` require users to link their project directory with
cloud storage during the package's inaugural use. This entails obtaining
explicit user consent.

- **dontshow**: Consequently, examples where this linkage would activate are 
wrapped in `\dontshow` conditional on `interactive()`.

- **Read Functions**: These initially pull files from the cloud to a temp folder
for reading, leaving the working directory untouched. In examples, the working
directory is also untouched.

- **Write Functions**: Files are first created in a temp folder, then sent to 
the cloud. The working directory remains untouched.

- **Download Functions**: These do pull files into the working directory, but
this is their primary purpose and they cannot write anywhere outside of it.
Also, in examples (shielded with `\dontshow`), I've added code to remove the
donloaded files.

- **Upload Functions**: In examples, files are generated files in the working
directory for uploading purposes. Still, cleanup code ensures their removal
afterward.

- **Vignettes**: Chunks using `cloudfs` functions aren't executed; they're all 
tagged with `eval=FALSE`.

- **Tests**: These only operate when Google Drive or S3 tokens are available,
excluding execution on CRAN. When testing, I use temporary folders for project
creation and employ `withr::with_dir` to execute `cloudfs` code — a strategy
suitable for testing but not for example clarity.

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.
