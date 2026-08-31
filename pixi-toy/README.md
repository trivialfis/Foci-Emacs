# Pixi Python Toy Project

This project exercises the Emacs pixi environment detector.

The `foci.toml` file selects the Pixi manifest and environment:

```toml
[pixi]
manifest-path = "pixi.toml"
environment = "default"
```

Run `pixi install` in this directory, then open `hello.py` in Emacs. The Python configuration should resolve `.pixi/envs/default/bin/python` and make tools such as `ruff` and `pylsp` available from the same pixi environment. ESS uses `.pixi/envs/default/bin/R` when the environment contains R.
