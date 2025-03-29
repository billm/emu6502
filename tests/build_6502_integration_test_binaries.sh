#!/bin/sh

# Using xa65 to assemble
xa -o helloworld helloworld.s

# Using acme to assemble
acme bf6502.s
