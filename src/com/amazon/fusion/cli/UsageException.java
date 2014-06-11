// Copyright (c) 2014 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion.cli;

/**
 * Indicates that an argument to the CLI is malformed or otherwise unusable.
 */
@SuppressWarnings("serial")
class UsageException
    extends Exception
{
    final Command myCommand;

    UsageException(Command command, String message)
    {
        super(message);
        myCommand = command;
    }

    UsageException(String message)
    {
        this(null, message);
    }
}
