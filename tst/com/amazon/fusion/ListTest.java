// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonList;
import org.junit.Assert;
import org.junit.Test;

/**
 *
 */
public class ListTest
    extends CoreTestCase
{
    @Test
    public void testAdd()
        throws Exception
    {
        assertEval("[1]", "(add null.list 1)");
        assertEval("[1]", "(add [] 1)");

        assertEval("(1)", "(add (quote null.sexp) 1)");
        assertEval("(1)", "(add (quote ()) 1)");
    }

    @Test
    public void testAddAnyIon()
        throws Exception
    {
        for (String form : allIonExpressions())
        {
            String expr = "(add [] " + form + ")";
            IonList result = (IonList) evalToIon(expr);
            Assert.assertEquals(1, result.size());
        }
    }

    @Test
    public void testDeepAdd()
        throws Exception
    {
        assertEval("{f:[2]}",
                   "(let ((s {f:[]}))" +
                   "  (add (. s \"f\") 2)" +
                   "  s)");

        assertEval("{f:(2)}",
                   "(let ((s {f:(quote ())}))" +
                   "  (add (. s \"f\") 2)" +
                   "  s)");
    }

    @Test
    public void testAddArity()
        throws Exception
    {
        expectArityFailure("(add)");
    }


    @Test
    public void testAddBadType()
        throws Exception
    {
        for (String form : nonSequenceExpressions())
        {
            String expr = "(add " + form + " 12)";
            expectArgTypeFailure(expr);
        }

        for (String form : nonIonExpressions())
        {
            String expr = "(add [] " + form + ")";
            expectArgTypeFailure(expr);
        }
    }


    //========================================================================


    @Test
    public void testSize()
        throws Exception
    {
        assertEval("0", "(size null.list)");
        assertEval("0", "(size [])");
        assertEval("1", "(size [1])");
        assertEval("2", "(size [2,2])");
    }


    @Test
    public void testSizeArity()
        throws Exception
    {
        expectArityFailure("(size)");
        expectArityFailure("(size [] [])");
    }

    @Test
    public void testSizeBadType()
        throws Exception
    {
        for (String form : nonListExpressions())
        {
            String expr = "(size " + form + ")";
            expectArgTypeFailure(expr);
        }
    }
}
