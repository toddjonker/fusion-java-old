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
    public void testSize()
        throws Exception
    {
        assertEval(0, "(size null.struct)");
        assertEval(0, "(size {})");
        assertEval(1, "(size {f:1})");
        assertEval(2, "(size {f:2, f:2})");  // repeated field
        assertEval(2, "(size {f:2, g:2})");
    }


    //========================================================================


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
            expectArgTypeFailure(expr, 0);

            expr = "(remove " + form + " \"f\")";
            expectArgTypeFailure(expr, 0);
        }
    }

    @Test
    public void testRemoveBadName()
        throws Exception
    {
        for (String form : nonTextExpressions())
        {
            String expr = "(remove {} " + form + ")";
            expectArgTypeFailure(expr, 1);

            expr = "(remove {} \"f\" " + form + " \"f\")";
            expectArgTypeFailure(expr, 2);
        }
    }


    //========================================================================


    @Test
    public void testForEachField()
        throws Exception
    {
        eval("(define add_name (lambda (name value) (add value name)))");
        assertEval("{f:[\"f\"],g:[\"g\"],f:[true,false,\"f\"]}",
                   "(for_each_field add_name " +
                   "  {f:null.list,g:[],f:[true,false]})");
    }

    @Test
    public void testForEachFieldArity()
        throws Exception
    {
        eval("(define add_name (lambda (name value) (add value name)))");
        expectArityFailure("(for_each_field)");
        expectArityFailure("(for_each_field add_name)");
        expectArityFailure("(for_each_field add_name {} 3)");
    }

    @Test
    public void testForEachFieldArgTypes()
        throws Exception
    {
        eval("(define add_name (lambda (name value) (add value name)))");

        for (String form : allIonExpressions())
        {
            String expr = "(for_each_field " + form + " {})";
            expectArgTypeFailure(expr, 0);
        }

        for (String form : nonStructExpressions())
        {
            String expr = "(for_each_field add_name " + form + ")";
            expectArgTypeFailure(expr, 1);
        }
    }
}
