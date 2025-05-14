# Interface Resolver Example

This example demonstrates how to use the `@custom:binds-to` NatSpec annotation to generate binding files for interface resolution.

## Files

- `IGreeter.sol`: Interface with `@custom:binds-to` annotation
- `FriendlyGreeter.sol`: First implementation of the IGreeter interface
- `FormalGreeter.sol`: Second implementation of the IGreeter interface
- `CallerContract.sol`: Contract that uses the IGreeter interface without knowing the specific implementation

## How it works

1. The `CallerContract` uses the `IGreeter` interface to make calls without knowing which specific implementation it will use.
2. The `@custom:binds-to GreeterImplementation` annotation in both the interface and the caller contract indicates that a binding is needed.
3. Running `sol2bnd` on this project will generate a skeleton binding file with the key "GreeterImplementation".
4. By editing the generated binding file, you can specify which implementation (`FriendlyGreeter` or `FormalGreeter`) should be used.

## Using sol2bnd

To generate the binding file:

```bash
cargo run --bin sol2bnd -- --project-path ./examples
```

This will create a `binding.yaml` file that you can edit to specify the implementation.

## Example binding.yaml

After running sol2bnd, you can edit the binding.yaml to look like this:

```yaml
bindings:
  - key: GreeterImplementation
    contract_name: FriendlyGreeter  # or FormalGreeter
    address: "0x123..."  # Optional contract address
    chain_id: 1  # Optional chain ID
    notes: "Binding for the Greeter interface"
```

This tells the analyzer that when it encounters the `IGreeter` interface or the `greeter` variable in `CallerContract`, it should use the `FriendlyGreeter` implementation for analysis.
