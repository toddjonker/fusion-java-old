// Copyright (c) 2014 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

/**
 * EXPERIMENTAL extension point for collecting code-coverage statistics.
 * <p>
 * At compile time, {@link #coverableLocation(SourceLocation)} is called once
 * for each (runtime) code point for which a location is known.  The collector
 * may constrain the extent of coverage metrics by filtering based on location:
 * a {@code false} result indicates that the code point should not count
 * evaluations.
 * <p>
 * At run time, {@link #coverLocation(SourceLocation)} is called each time
 * time a coverable code point is (about to be) evaluated.
 */
public interface _Private_CoverageCollector
{
    /**
     * Notes that a code point is associated with a given source location.
     * This method may be called more than once with the same location and
     * the return value is expected to be the same for such calls.
     * <p>
     * This method is called during compilation, so the code point hasn't been
     * evaluated yet (and may never be evaluated).
     *
     * @return whether the compiler should record coverage for the location.
     * If false, then {@link #coverableLocation(SourceLocation)} will never be
     * called with the given location.
     */
    boolean coverableLocation(SourceLocation loc);

    /**
     * Indicates that a code point associated wth a location is about to be
     * evaluated.
     * This method will be called once for each evaluation associated with the
     * location.
     */
    void coverLocation(SourceLocation loc);


    // TODO Think this through. I'm not really convinced this is correct.
    //      Perhaps close() would be better?  save()?
    void flushMetrics()
        throws FusionException;
}
