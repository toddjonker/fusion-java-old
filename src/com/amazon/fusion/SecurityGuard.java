// Copyright (c) 2022 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

/**
 * Gates access to system resources.
 *
 * This implementation is extremely quick-and-dirty.
 *
 * See https://docs.racket-lang.org/reference/securityguards.html
 */
final class SecurityGuard
{
    static final SecurityGuard OPEN   = new SecurityGuard(true);
    static final SecurityGuard CLOSED = new SecurityGuard(false);


    private final boolean myFileSystemEnabled;

    private SecurityGuard(boolean allowFileSystemAccess)
    {
        myFileSystemEnabled = allowFileSystemAccess;
    }


    boolean isFileSystemEnabled()
    {
        return myFileSystemEnabled;
    }
}
