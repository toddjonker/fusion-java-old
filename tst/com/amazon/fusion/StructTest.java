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


    //========================================================================


    @Test @Ignore
    public void testDeepRemoveKeysM()
        throws Exception
    {
        assertEval("{f:1,g:{i:4}}",
                   "(let ((s {f:1,g:{h:3,i:4}}))" +
                   "  (remove_keys_m (. s \"g\") \"h\")" +
                   "  s)");
    }

    @Test
    public void testRemoveKeysArity()
        throws Exception
    {
        expectArityFailure("(remove_keys)");
    }

    @Test
    public void testRemoveKeysBadStruct()
        throws Exception
    {
        for (String form : nonStructExpressions())
        {
            String expr = "(remove_keys " + form + ")";
            expectArgTypeFailure(expr, 0);

            expr = "(remove_keys " + form + " \"f\")";
            expectArgTypeFailure(expr, 0);
        }
    }

    @Test
    public void testRemoveKeyBadName()
        throws Exception
    {
        for (String form : nonTextExpressions())
        {
            String expr = "(remove_keys {} " + form + ")";
            expectArgTypeFailure(expr, 1);

            expr = "(remove_keys {} \"f\" " + form + " \"f\")";
            expectArgTypeFailure(expr, 2);
        }
    }


    @Test
    public void retainKeyFail()
        throws Exception
    {
        expectArityFailure("(retain_keys)");

        expectContractFailure("(retain_keys [\"a\"] \"a\")");
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
         assertEval("{}",
                    "(struct_make)");
         assertEval("{f:3}",
                    "(struct_make \"f\" 3)");
         assertEval("{hello:\"world\"}",
                    "(struct_make \"hello\" \"world\")");
         assertEval("{f:3,g:\"hello\"}",
                    "(struct_make \"f\" 3 \"g\" \"hello\")");
         assertEval("{A:3,B:[true,false,[]]}",
                    "(struct_make \"A\" 3 \"B\" [true,false,[]])");
         assertEval("{hello:\"world\", hello:\"hello\"}",
                    "(struct_make \"hello\" \"hello\" \"hello\" \"world\")");
         assertEval("{a:1,a:2,b:3}",
                    "(struct_make \"a\" 1 \"b\" 3 \"a\" 2)");
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
    public void structMergeFail()
        throws Exception
    {
        expectArityFailure("(struct_merge)");
        expectArityFailure("(struct_merge {f:3})");

        expectArgTypeFailure("(struct_merge {f:3} 4)",1);
    }
}
