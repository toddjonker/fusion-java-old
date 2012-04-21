// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import org.junit.Test;

/**
 *
 */
public class StructTest
    extends CoreTestCase
{
    @Test
    public void testRemove()
        throws Exception
    {
        assertEval("null.struct", "(remove null.struct)");
        assertEval("null.struct", "(remove null.struct \"f\")");

        assertEval("{}", "(remove {})");
        assertEval("{}", "(remove {} \"f\")");
        assertEval("{}", "(remove {f:1} \"f\")");
        assertEval("{g:2}", "(remove {g:2,f:1} \"f\")");
        assertEval("{}", "(remove {f:1,g:2} \"f\" (quote g))");
    }

    @Test
    public void testDeepRemove()
        throws Exception
    {
        assertEval("{f:1,g:{i:4}}",
                   "(let ((s {f:1,g:{h:3,i:4}}))" +
                   "  (remove (. s \"g\") \"h\")" +
                   "  s)");
    }

    @Test
    public void testRemoveArity()
        throws Exception
    {
        expectArityFailure("(remove)");
    }

    @Test
    public void testRemoveBadStruct()
        throws Exception
    {
        for (String form : nonStructExpressions())
        {
            String expr = "(remove " + form + ")";
            expectArgTypeFailure(expr);

            expr = "(remove " + form + " \"f\")";
            expectArgTypeFailure(expr);
        }
    }

    @Test
    public void testRemoveBadName()
        throws Exception
    {
        for (String form : nonTextExpressions())
        {
            String expr = "(remove {} " + form + ")";
            expectArgTypeFailure(expr);

            expr = "(remove {} \"f\" " + form + " \"f\")";
            expectArgTypeFailure(expr);
        }
    }


    @Test
    public void testForEachChild()
        throws Exception
    {
        eval("(define add_name (func (name value) (add value name)))");
        assertEval("{f:[\"f\"],g:[\"g\"],f:[true,false,\"f\"]}",
                   "(for_each_field add_name " +
                   "  {f:null.list,g:[],f:[true,false]})");
    }
}
