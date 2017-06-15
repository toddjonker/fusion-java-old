// Copyright (c) 2017 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static org.junit.Assert.fail;
import java.util.Iterator;
import org.junit.Test;

public class StackTraceTest
    extends CoreTestCase
{
    private Iterator<SourceLocation> myStack;


    private void evalForTrace(String code)
        throws Exception
    {
        FusionException e = expectFailure(FusionException.class, code);
        myStack = e.getContextLocations().iterator();
    }


    private SourceLocation popLocation()
    {
        SourceLocation loc = myStack.next();

        // Ignore library code, which all has SourceName attached.
        while (loc.getSourceName() != null)
        {
            loc = myStack.next();
        }
        return loc;
    }


    private void expectLocation(int line, int column)
    {
        SourceLocation loc = popLocation();
        if (line != loc.getLine() || column != loc.getColumn())
        {
            fail("Expected L" + line +" C" + column
                     + " found L" + loc.getLine() + " C" + loc.getColumn());
        }
    }


    //========================================================================


    @Test

    public void testLet1()
        throws Exception
    {
        evalForTrace("(let [(a (first 1))] a)");
        expectLocation(1, 10);
        expectLocation(1, 1);

        evalForTrace("(let loop\n" +
                     "   [(a (first 1))] a)");
        expectLocation(2, 8);
        expectLocation(1, 1);
    }

    @Test
    public void testLet2()
        throws Exception
    {
        evalForTrace("(let [(a  (first 1)),\n" +
                     "      (b 2)]\n" +
                     "  a)");
        expectLocation(1, 11);
        expectLocation(1, 1);

        evalForTrace("(let [(a 1),\n" +
                     "      (b   (first 1))]\n" +
                     "  a)");
        expectLocation(2, 12);
        expectLocation(1, 1);
    }

    @Test
    public void testLet3()
        throws Exception
    {
        evalForTrace("(let [(a (first 1)),\n" +
                     "      (b 2),\n" +
                     "      (c 3)]\n" +
                     "  a)");
        expectLocation(1, 10);
        expectLocation(1, 1);

        evalForTrace("(let [(a 1),\n" +
                     "      (b   (first 1)),\n" +
                     "      (c 3)]\n" +
                     "  a)");
        expectLocation(2, 12);
        expectLocation(1, 1);

        evalForTrace("(let [(a 1),\n" +
                     "      (b 2),\n" +
                     "      (c    (first 1))]\n" +
                     "  a)");
        expectLocation(3, 13);
        expectLocation(1, 1);
    }
}
