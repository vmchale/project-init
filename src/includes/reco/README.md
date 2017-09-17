# Example

## Structure

The kernel (that is, the code that runs on the FPGA) is located at `main.go`.
The test command is located at `cmd/test-{{ project }}/main.go`.

## Testing

To test:

```
reco test run test-{{ project }}
```

This will simulate the kernel using a hardware simulator, and test it
using the `test-{{ project }}` command.

## Building

```
reco build run test-{{ project }}
```

This will build your commands and kernel.
