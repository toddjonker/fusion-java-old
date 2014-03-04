// Copyright (c) 2014 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;


/**
 * Internal exception thrown when a Fusion evaluation is interrupted via
 * {@link Thread#interrupt()}.
 * <p>
 * This does not extend {@link FusionException}! If it did, we'd have to be
 * careful to handle the interrupt exception everywhere we catch
 * {@link FusionException}, or else the interrupt could easily be swallowed.
 * <p>
 * Instead, we throw an {@link Error} that
 * is much less likely to be mis-handled. We catch it at entry points to the
 * evaluator and wrap it in the public sibling exception
 * {@link FusionInterruptedException}.
 */
@SuppressWarnings("serial")
final class FusionInterrupt
    extends Error
{
    FusionInterrupt()
    {
        super("The Fusion evaluation thread was interrupted.");
    }
}
