if {$argc == 1} {
  set project_name [lindex $argv 0]
}
if {$argc < 1} {
  set project_name "INet"
}

open_project ${project_name}/${project_name}.gprj

run all
