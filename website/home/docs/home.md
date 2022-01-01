## Download

### For macOS

These steps work for both Intel and Apple Silicon Macs.

1. Open Terminal.

2. Install [Homebrew](https://brew.sh) if needed.

3. Copy and paste the following command:

    ```shell
    brew tap wipplelang/wipple && brew install wipple --build-from-source
    ```

### For Linux

1. Download a binary from [Releases](https://github.com/wipplelang/wipple/releases/latest).

2. Move the binary to a location of your choice, eg. `/usr/local/bin/wipple`, and `chmod +x` it.

## Hello, world!

Make sure things are set up by running this command:

```shell
wipple run <(echo 'show "Hello, world!"')
```

## Next steps

Check out the [Wipple Documentation](https://docs.wipple.gramer.dev) to learn how to write Wipple code, set up a project, and more!
