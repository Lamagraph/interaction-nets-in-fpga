# interaction-nets-in-fpga-core

Interaction nets based processor in Clash

## Build and synthesis

To make Gowin project you can run:

```bash
./gprj-processor Core.Core <device-name:mega138|primer25> <proj-name> -m create
```

Then you need to [add IP-core](https://www.gowinsemi.com/upload/database_doc/3/document/5bfcfde43c45c.pdf) of UART Master in your design.

After that all pipline is

```bash
./gprj-processor Core.Core <device-name:mega138|primer25> <proj-name> -m pnr --get_uart_out
```
