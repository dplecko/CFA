
load_py_deps <- function() {

  script_path <- system.file("python", "neural-learning.py",
                             package = "faircause")
  reticulate::source_python(script_path)
}
