//! Dependency free terminal size detection.
//!
//! This declares the few system functions needed directly instead of
//! depending on `libc` or `windows-sys`.

/// Returns the width of the terminal attached to stdout in columns.
///
/// Returns `None` if stdout is not a terminal (eg: when piped) or the
/// platform is not supported.  Only stdout is checked as this is where help
/// is printed to and it keeps redirected output deterministic.
pub(crate) fn terminal_width() -> Option<usize> {
    imp::terminal_width().filter(|&x| x > 0)
}

#[cfg(unix)]
mod imp {
    use std::ffi::c_int;

    #[repr(C)]
    #[derive(Default)]
    struct Winsize {
        ws_row: u16,
        ws_col: u16,
        ws_xpixel: u16,
        ws_ypixel: u16,
    }

    // The type of the request argument differs between C libraries.
    #[cfg(any(
        target_env = "musl",
        target_os = "android",
        target_os = "illumos",
        target_os = "solaris"
    ))]
    type Request = c_int;
    #[cfg(not(any(
        target_env = "musl",
        target_os = "android",
        target_os = "illumos",
        target_os = "solaris"
    )))]
    type Request = std::ffi::c_ulong;

    // Linux uses its own numbering on most architectures, except for the
    // ones that inherited the BSD style ioctl encoding.
    #[cfg(all(
        any(target_os = "linux", target_os = "android"),
        not(any(
            target_arch = "mips",
            target_arch = "mips64",
            target_arch = "powerpc",
            target_arch = "powerpc64",
            target_arch = "sparc",
            target_arch = "sparc64"
        ))
    ))]
    const TIOCGWINSZ: Option<Request> = Some(0x5413);

    // _IOR('t', 104, struct winsize)
    #[cfg(any(
        all(
            any(target_os = "linux", target_os = "android"),
            any(
                target_arch = "mips",
                target_arch = "mips64",
                target_arch = "powerpc",
                target_arch = "powerpc64",
                target_arch = "sparc",
                target_arch = "sparc64"
            )
        ),
        target_vendor = "apple",
        target_os = "freebsd",
        target_os = "netbsd",
        target_os = "openbsd",
        target_os = "dragonfly",
    ))]
    const TIOCGWINSZ: Option<Request> = Some(0x40087468);

    // ('T' << 8) | 104
    #[cfg(any(target_os = "illumos", target_os = "solaris"))]
    const TIOCGWINSZ: Option<Request> = Some(0x5468);

    #[cfg(not(any(
        target_os = "linux",
        target_os = "android",
        target_vendor = "apple",
        target_os = "freebsd",
        target_os = "netbsd",
        target_os = "openbsd",
        target_os = "dragonfly",
        target_os = "illumos",
        target_os = "solaris",
    )))]
    const TIOCGWINSZ: Option<Request> = None;

    extern "C" {
        fn ioctl(fd: c_int, request: Request, ...) -> c_int;
    }

    pub(super) fn terminal_width() -> Option<usize> {
        let request = TIOCGWINSZ?;
        let mut size = Winsize::default();
        // SAFETY: TIOCGWINSZ writes a `struct winsize` into the pointer
        // which matches the layout of `Winsize`.
        let rv = unsafe { ioctl(1, request, &mut size as *mut Winsize) };
        (rv == 0 && size.ws_col > 0).then_some(size.ws_col as usize)
    }
}

#[cfg(windows)]
mod imp {
    use std::ffi::c_void;

    type Handle = *mut c_void;

    const STD_OUTPUT_HANDLE: u32 = -11i32 as u32;

    #[repr(C)]
    #[derive(Default)]
    struct Coord {
        x: i16,
        y: i16,
    }

    #[repr(C)]
    #[derive(Default)]
    struct SmallRect {
        left: i16,
        top: i16,
        right: i16,
        bottom: i16,
    }

    #[repr(C)]
    #[derive(Default)]
    struct ConsoleScreenBufferInfo {
        size: Coord,
        cursor_position: Coord,
        attributes: u16,
        window: SmallRect,
        maximum_window_size: Coord,
    }

    #[link(name = "kernel32")]
    extern "system" {
        fn GetStdHandle(std_handle: u32) -> Handle;
        fn GetConsoleScreenBufferInfo(
            console_output: Handle,
            console_screen_buffer_info: *mut ConsoleScreenBufferInfo,
        ) -> i32;
    }

    pub(super) fn terminal_width() -> Option<usize> {
        // SAFETY: GetStdHandle has no preconditions.
        let handle = unsafe { GetStdHandle(STD_OUTPUT_HANDLE) };
        if handle.is_null() || handle as isize == -1 {
            return None;
        }
        let mut info = ConsoleScreenBufferInfo::default();
        // SAFETY: the handle was checked and `info` matches the
        // layout of CONSOLE_SCREEN_BUFFER_INFO.
        if unsafe { GetConsoleScreenBufferInfo(handle, &mut info) } == 0 {
            return None;
        }
        let width = info.window.right as i32 - info.window.left as i32 + 1;
        usize::try_from(width).ok()
    }
}

#[cfg(not(any(unix, windows)))]
mod imp {
    pub(super) fn terminal_width() -> Option<usize> {
        None
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_terminal_width_does_not_crash() {
        // under cargo test the streams are usually not a terminal, so we can
        // only check that the call is sound and returns a sensible value.
        if let Some(width) = super::terminal_width() {
            assert!(width > 0 && width < 10000);
        }
    }
}
