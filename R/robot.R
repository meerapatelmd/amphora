#' @title
#' Install Robot
#'
#' @description
#' Downloads a jar and shell script to $PATH (`path`). Once complete, cd into
#' the path and run `sudo chmod u+x robot` in the terminal to make ROBOT
#' executable. Learn more at http://robot.obolibrary.org/.
#'
#' @export
#' @rdname install_robot


install_robot <-
        function(jar_url = "https://github.com/ontodev/robot/releases/download/v1.7.2/robot.jar",
                 sh_url = "https://raw.githubusercontent.com/ontodev/robot/master/bin/robot",
                 path = "/usr/local/bin") {

                jar_file <- file.path(path, "robot.jar")

                if (!file.exists(jar_file)) {
                download.file(url = jar_url,
                              destfile = jar_file)
                }

                sh_file <- file.path(path, "robot")

                if (!file.exists(sh_file)) {
                download.file(url = sh_url,
                              destfile = sh_file)

                }


        }

#' @title
#' Convert an Ontology Format
#'
#' @description
#' Supported extensions include:
#' \itemize{
#'   \item json
#'   \item obo
#'   \item ofn
#'   \item omn
#'   \item owl
#'   \item owx
#'   \item ttl
#' }
#'
#' See \url{http://robot.obolibrary.org/convert} for more details.
#'
#' @export
#' @rdname robot_convert

robot_convert <-
        function(input,
                 output) {


                system(command = sprintf("robot convert --input %s --output %s", input, output))

        }



