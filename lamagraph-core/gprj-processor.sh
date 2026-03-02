#!/bin/bash


usage() {
    echo "Usage: $0 [-h] [-m mode] [-get_uart_out] <clash-module> <device-name> <proj-name>"
    echo "  -h    show this help"
    echo "  -m    choose one of the build mode:
                      1) all    -- crate project and run PnR
                      2) pnr    -- only run PnR
                      3) create -- only create project
                  default is 'all'"
    exit 1
}

mode="all"
getOut="false"

OPTIONS=$(getopt -o hm:g -l help,mode:,get_uart_out -n "$0" -- "$@")
if [ $? != 0 ]; then
    usage
fi

eval set -- "$OPTIONS"

while true; do
    case "$1" in
        -h|--help)
            usage
            ;;
        -m|--mode)
            case "$2" in
                all|pnr|create)
                    mode="$2"
                    shift 2
                    ;;
                *)
                    echo "Wrong mode option: $2"
                    usage
                    ;;
            esac
            ;;
        -g|--get_uart_out)
            getOut="true"
            shift
            ;;
        --)
            shift
            break
            ;;
        *)
            echo "Unknown option: $1"
            usage
            ;;
    esac
done


clashModule="$1"
device="$2"
gprjName="$3"

stack run -- clash $clashModule --systemverilog -fclash-clear -fclash-hdldir hdl/systemverilog


case $mode in
    all)
        gw_sh hdl/make-project.tcl -name $gprjName -board $device
        gw_sh hdl/run-pnr.tcl $gprjName
        openFPGALoader  $gprjName/impl/pnr/$gprjName.fs
        ;;
    create)
        gw_sh hdl/make-project.tcl -name $gprjName -board $device
        ;;
    pnr)
        gw_sh hdl/run-pnr.tcl $gprjName
        openFPGALoader  $gprjName/impl/pnr/$gprjName.fs
        ;;
esac

if [ "$getOut" = "true" ]; then
    timeout 10 minicom -D /dev/ttyUSB0-H --baud 9600 -C uart_data_$(date +%Y-%m-%d_%H:%M)
fi
