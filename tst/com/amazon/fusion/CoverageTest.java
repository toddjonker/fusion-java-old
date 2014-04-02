// Copyright (c) 2014 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import java.util.HashSet;
import java.util.Set;
import org.junit.Test;

/**
 *
 */
public class CoverageTest
    extends CoreTestCase
{
    static class Collector
        implements _Private_CoverageCollector
    {
        boolean coverOnlyLineZero = false;

        Set<SourceLocation> coverable = new HashSet<>();
        Set<SourceLocation> covered   = new HashSet<>();

        @Override
        public boolean coverableLocation(SourceLocation loc)
        {
            coverable.add(loc);
            return (coverOnlyLineZero ? loc.getLine() == 0 : true);
        }

        @Override
        public void coverLocation(SourceLocation loc)
        {
            covered.add(loc);
        }
    }


    private final Collector collector = new Collector();


    private void checkCovered(SourceName name, long line, long column)
    {
        SourceLocation loc = SourceLocation.forLineColumn(name, line, column);
        assertTrue(collector.coverable.contains(loc));
        assertTrue(collector.covered.contains(loc));
    }

    private void checkCovered(long line, long column)
    {
        checkCovered(null, line, column);
    }


    private void checkNotCovered(SourceName name, long line, long column)
    {
        SourceLocation loc = SourceLocation.forLineColumn(name, line, column);
        assertTrue("not coverable",
                   collector.coverable.contains(loc));
        assertFalse("covered but shouldn't be",
                    collector.covered.contains(loc));
    }

    private void checkNotCovered(long line, long column)
    {
        checkNotCovered(null, line, column);
    }



    @Override
    protected FusionRuntimeBuilder runtimeBuilder()
        throws FusionException
    {
        FusionRuntimeBuilder b = super.runtimeBuilder();

        b.setCoverageCollector(collector);

        return b;
    }


    @Test
    public void testCollection()
        throws FusionException
    {
        TopLevel top = topLevel();

        eval("0");
        checkCovered(0, 0);

        //    0   4 6  9 11
        eval("(if true\n" +
             "    1 2)");
        checkCovered(0, 0);
        checkCovered(0, 4);
        checkCovered(1, 4);
        checkNotCovered(1, 6);

        SourceName name1 = SourceName.forDisplay("define");
        //        0 2 4 6 8
        top.eval("(define (f t)\n" +
                 "  (if t      \n" +
                 "      1      \n" +
                 "      2))",
                 name1);
        checkCovered(name1, 0, 0);
        checkNotCovered(name1, 1, 2);
        checkNotCovered(name1, 1, 6);
        checkNotCovered(name1, 2, 6);
        checkNotCovered(name1, 3, 6);

        top.call("f", true);
        checkCovered(name1, 1, 2);
        checkCovered(name1, 1, 6);
        checkCovered(name1, 2, 6);
        checkNotCovered(name1, 3, 6);

        SourceName name2 = SourceName.forDisplay("invoke");
        //        0 2 4 6 8
        top.eval("(f false)",
                 name2);
        checkCovered(name2, 0, 0);
        checkCovered(name2, 0, 1);
        checkCovered(name2, 0, 3);
        checkCovered(name1, 3, 6);
    }

    @Test
    public void testPartialCollection()
        throws FusionException
    {
        collector.coverOnlyLineZero = true;

        //    0   4 6  9 11
        eval("(if true\n" +
             "    1 2)");
        checkCovered(0, 0);
        checkCovered(0, 4);
        checkNotCovered(1, 4);
        checkNotCovered(1, 6);
    }
}
