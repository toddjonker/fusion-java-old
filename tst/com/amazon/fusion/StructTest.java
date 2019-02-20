// Copyright (c) 2012-2019 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionStruct.asImmutableStruct;
import static com.amazon.fusion.FusionStruct.isImmutableStruct;
import static com.amazon.fusion.FusionStruct.isMutableStruct;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertTrue;
import org.junit.Before;
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


    //========================================================================


    @Test
    public void testDeepRemoveKeysM()
        throws Exception
    {
        assertEval("{f:1,g:{i:4}}",
                   "(let [(s (mutable_struct \"f\" 1 " +
                   "           \"g\" (mutable_struct \"h\" 3 \"i\" 4)))]" +
                   "  (remove_keys_m (. s \"g\") \"h\")" +
                   "  s)");
    }

    @Test
    public void testRemoveKeysArity()
        throws Exception
    {
        expectArityExn("(remove_keys)");
    }

    @Test
    public void testRemoveKeysBadStruct()
        throws Exception
    {
        for (String form : nonStructExpressions())
        {
            String expr = "(remove_keys " + form + ")";
            expectArgumentExn(expr, 0);

            expr = "(remove_keys " + form + " \"f\")";
            expectArgumentExn(expr, 0);
        }
    }

    @Test
    public void testRemoveKeyBadName()
        throws Exception
    {
        for (String form : nonTextExpressions())
        {
            String expr = "(remove_keys {} " + form + ")";
            expectArgumentExn(expr, 1);

            expr = "(remove_keys {} \"f\" " + form + " \"f\")";
            expectArgumentExn(expr, 2);
        }
    }


    @Test
    public void retainKeyFail()
        throws Exception
    {
        expectArityExn("(retain_keys)");

        expectContractExn("(retain_keys [\"a\"] \"a\")");
    }

    //========================================================================


    @Test
    public void testStructForEach()
        throws Exception
    {
        eval("(define add_name (lambda (name value) (add_m value name)))");
        assertEval("{f:[f],g:[g],f:[true,false,f]}",
                   "(struct_for_each add_name " +
                   "  (mutable_struct \"f\" (stretchy_list) " +
                   "                  \"g\" (stretchy_list) " +
                   "                  \"f\" (stretchy_list true false)))");
    }

    @Test
    public void testStructForEachArity()
        throws Exception
    {
        eval("(define add_name (lambda (name value) (add value name)))");
        expectArityExn("(struct_for_each)");
        expectArityExn("(struct_for_each add_name)");
        expectArityExn("(struct_for_each add_name {} 3)");
    }

    @Test
    public void testStructForEachArgTypes()
        throws Exception
    {
        eval("(define add_name (lambda (name value) (add value name)))");

        for (String form : allIonExpressions())
        {
            String expr = "(struct_for_each " + form + " {})";
            expectArgumentExn(expr, 0);
        }

        for (String form : nonStructExpressions())
        {
            String expr = "(struct_for_each add_name " + form + ")";
            expectArgumentExn(expr, 1);
        }
    }

    @Test
    public void makeStruct()
        throws Exception
    {
         assertEval("{}",
                    "(struct)");
         assertEval("{f:3}",
                    "(struct \"f\" 3)");
         assertEval("{hello:\"world\"}",
                    "(struct (quote hello) \"world\")");
         assertEval("{f:3,g:\"hello\"}",
                    "(struct \"f\" 3 (quote g) \"hello\")");
         assertEval("{A:3,B:[true,false,[]]}",
                    "(struct \"A\" 3 \"B\" [true,false,[]])");
         assertEval("{hello:\"world\", hello:\"hello\"}",
                    "(struct \"hello\" \"hello\" \"hello\" \"world\")");
         assertEval("{a:1,a:2,b:3}",
                    "(struct \"a\" 1 \"b\" 3 \"a\" 2)");
    }

    @Test
    public void makeStructFail()
        throws Exception
    {
        expectContractExn("(struct \"hello\")");
        expectContractExn("(struct \"hello\" 13 \"world\")");

        expectArgumentExn("(struct 15 14)",0);
    }


    @Test
    public void structZipFail()
        throws Exception
    {
        eval("(define elemList [1,2,3])");
        expectArityExn("(struct_zip)");
        expectArityExn("(struct_zip elemList)");
        expectArityExn("(struct_zip elemList elemList elemList)");

        expectContractExn("(struct_zip [\"hello\",2,\"world\"] elemList)");
    }


    @Test
    public void structMergeFail()
        throws Exception
    {
        expectArityExn("(struct_merge)");
        expectArityExn("(struct_merge {f:3})");

        expectArgumentExn("(struct_merge {f:3} 4)",1);
    }


    @Test
    public void testMakeMutableStruct()
        throws Exception
    {
        Evaluator eval = evaluator();

        Object ms = FusionStruct.mutableStruct(eval);
        assertTrue(isMutableStruct(eval, ms));
        assertEquals(0, FusionStruct.unsafeStructSize(eval, ms));
    }


    @Test
    public void testAsImmutableStruct()
        throws Exception
    {
        Evaluator eval = evaluator();

        Object ms = eval("(mutable_struct \"a\" 1 \"b\" 2)");
        checkIon(system().singleValue("{a:1,b:2}"), ms);
        assertTrue(isMutableStruct(eval, ms));

        Object is = asImmutableStruct(eval, ms);
        assertNotSame(ms, is);
        assertTrue(isImmutableStruct(eval, is));
        checkIon(system().singleValue("{a:1,b:2}"), is);
    }
}
