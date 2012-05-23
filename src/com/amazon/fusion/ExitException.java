// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

/**
 * Thown to force the exit of an evaluation.
 */
@SuppressWarnings("serial")
public final class ExitException
    extends FusionException
{
    ExitException() { super("Exit requested"); }
}