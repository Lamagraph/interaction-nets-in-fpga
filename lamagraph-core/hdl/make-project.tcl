set board "primer25"
set project_name "INet"

for {set i 0} {$i < $argc} {incr i} {
    set arg [lindex $argv $i]
    switch -exact $arg {
        "-board" {
            incr i
            set board [lindex $argv $i]
        }
        "-name" {
            incr i
            set project_name [lindex $argv $i]
        }
        "-h" -
        "-help" {
            puts "Usage: gw_sh make-project.tcl \[-board <primer25|mega138>\] \[-name <project-name>\]"
            puts "  -board (default: primer25)"
            puts "  -name (default: INet)"
            exit 0
        }
        default {
            puts "Error: unknown argument '$arg'"
            exit 1
        }
    }
}

switch -exact $board {
    "primer25" {
        set device "GW5A-LV25MG121NES"
        set device_version "A"
        # set constraints_file "../lamagraph-core/hdl/primer25_pin_constraints.cst"
        set constraints_file "../hdl/primer25_pin_constraints.cst"
    }
    "mega138" {
        set device "GW5AST-LV138FPG676AES"
        set device_version "B"
        # set constraints_file "../lamagraph-core/hdl/mega138_pin_constraints.cst"
        set constraints_file "../hdl/mega138_pin_constraints.cst"
    }
    default {
        puts "Error: unknown board '$board'"
        puts "Expected: primer25, mega138"
        exit 1
    }
}

create_project -name $project_name -pn $device -device_version $device_version -force

set_option -output_base_name $project_name

add_file -type cst $constraints_file

set_option -use_cpu_as_gpio 1
set_option -synthesis_tool gowinsynthesis
set_option -top_module topEntity
set_option -verilog_std sysv2017

# add_file -type verilog ../lamagraph-core/hdl/systemverilog/Core.Core.topEntity/topEntity.sv
# add_file -type verilog ../lamagraph-core/hdl/systemverilog/Core.Core.topEntity/topEntity_types.sv
# add_file -type sdc ../lamagraph-core/hdl/systemverilog/Core.Core.topEntity/topEntity.sdc
add_file -type verilog ../hdl/systemverilog/Core.Core.topEntity/topEntity.sv
add_file -type verilog ../hdl/systemverilog/Core.Core.topEntity/topEntity_types.sv
add_file -type sdc ../hdl/systemverilog/Core.Core.topEntity/topEntity.sdc

puts "Project '$project_name' for '$board' are created"
