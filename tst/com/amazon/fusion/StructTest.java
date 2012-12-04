// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;

/**
 *
 */
public class StructTest
    extends CoreTestCase
{
    @Before
    public void requires()
        throws FusionException
    {
        topLevel().requireModule("/fusion/list");
        topLevel().requireModule("/fusion/struct");
    }


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


    @Test @Ignore  // FUSION-86
    public void testForEachField()
        throws Exception
    {
        eval("(define add_name (lambda (name value) (add_m value name)))");
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

    @Test
    public void makeStruct()
        throws Exception
    {
         assertEval("{}","(struct_make)");
         assertEval("{f:3}","(struct_make \"f\" 3)");
         assertEval("{hello:\"world\"}", "(struct_make \"hello\" \"world\")");
         assertEval("{f:3,g:\"hello\"}","(struct_make \"f\" 3 \"g\" \"hello\")");
         assertEval("{A:3,B:[true,false,[]]}","(struct_make \"A\" 3 \"B\" [true,false,[]])");
         assertEval("{hello:\"world\"}", "(struct_make \"hello\" \"hello\" \"hello\" \"world\")");
    }

    @Test
    public void makeStructFail()
        throws Exception
    {
        expectContractFailure("(struct_make \"hello\")");
        expectContractFailure("(struct_make \"hello\" 13 \"world\")");

        expectArgTypeFailure("(struct_make 15 14)",0);
    }

    @Test
    public void structZip()
        throws Exception
    {
        assertEval("{f:3}","(struct_zip [\"f\"] [3])");
        assertEval("{hello:\"world\"}", "(struct_zip [\"hello\"] [\"world\"])");
        assertEval("{f:3,g:\"hello\"}","(struct_zip [\"f\",\"g\"] [3,\"hello\"])");
        assertEval("{A:3,B:[true,false,[]]}","(struct_zip [\"A\",\"B\"] [3,[true,false,[]]])");
        assertEval("{A:3}","(struct_zip [\"A\",\"A\"] [[true,false,[]],3])");
        assertEval("{A:3}","(struct_zip [\"A\",\"B\"] [3])");
    }

    @Test
    public void structZipFail()
        throws Exception
    {
        eval("(define elemList [1,2,3])");
        expectArityFailure("(struct_zip)");
        expectArityFailure("(struct_zip elemList)");
        expectArityFailure("(struct_zip elemList elemList elemList)");

        expectContractFailure("(struct_zip [\"hello\",2,\"world\"] elemList)");
    }

    @Test
    public void structUnion()
        throws Exception
    {
        assertEval("{f:3}","(struct_union {f:3})");
        assertEval("{}","(struct_union)");
        assertEval("{}","(struct_union {} {})");
        assertEval("{f:4}","(struct_union {f:3} {f:4})");
        assertEval("{f:3,g:4}","(struct_union {f:3} {g:4})");
        assertEval("{f:3,g:4,h:5}","(struct_union {f:3} {g:4,h:5})");
    }

    @Test
    public void structUnionFail()
        throws Exception
    {
        expectArgTypeFailure("(struct_union {f:3} 4)",1);
    }


    @Test
    public void structPrune()
        throws Exception
    {
        eval("(define testStruct {a:1,b:3,c:2})");
        assertEval("{a:1,c:2}","(struct_prune testStruct \"a\" \"c\")");
        assertEval("{}","(struct_prune testStruct)");
        assertEval("{a:3}", "(struct_prune {a:3} \"a\")");
    }

    @Test
    public void structPruneFail()
        throws Exception
    {
        expectArityFailure("(struct_prune)");

        expectContractFailure("(struct_prune [\"a\"] \"a\")");
        expectContractFailure("(struct_prune {} \"a\")");
    }
}
