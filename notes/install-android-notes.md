# Install android on Ubuntu (18.04)

- Install sdk https://developer.android.com/studio/install
- Install "build tools"

https://developer.mozilla.org/en-US/docs/Mozilla/Developer_guide/Build_Instructions/Simple_Firefox_for_Android_build
Ubuntu 18.04 - https://stackoverflow.com/a/50688351 (but install java8 from oracle https://tecadmin.net/install-oracle-java-8-ubuntu-via-ppa/)
https://askubuntu.com/a/890304
ln -s /home/jonathan/.mozbuild/android-sdk-linux /home/jonathan/.mozbuild/android-sdk
rustup target add  armv7-linux-androideabi

```
sudo apt install qemu-kvm
sudo adduser <Replace with username> kvm
sudo chown <Replace with username> /dev/kvm
```

https://wiki.mozilla.org/Mobile/Fennec/Android/Testing
./mach android-emulator --force-update --version x86
rustup target add  i686-linux-android

Don't use --disable-optimize in .mozconfig https://bugzilla.mozilla.org/show_bug.cgi?id=1224871
