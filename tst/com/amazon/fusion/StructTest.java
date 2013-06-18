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


    @Test @Ignore // TODO FUSION-93 struct mutability
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


    @Test @Ignore  // TODO FUSION-93
    public void testStructForEach()
        throws Exception
    {
        eval("(define add_name (lambda (name value) (add_m value name)))");
        assertEval("{f:[\"f\"],g:[\"g\"],f:[true,false,\"f\"]}",
                   "(struct_for_each add_name " +
                   "  {f:null.list,g:[],f:[true,false]})");
        // TODO as written the struct and its field values are immutable.
    }

    @Test
    public void testStructForEachArity()
        throws Exception
    {
        eval("(define add_name (lambda (name value) (add value name)))");
        expectArityFailure("(struct_for_each)");
        expectArityFailure("(struct_for_each add_name)");
        expectArityFailure("(struct_for_each add_name {} 3)");
    }

    @Test
    public void testStructForEachArgTypes()
        throws Exception
    {
        eval("(define add_name (lambda (name value) (add value name)))");

        for (String form : allIonExpressions())
        {
            String expr = "(struct_for_each " + form + " {})";
            expectArgTypeFailure(expr, 0);
        }

        for (String form : nonStructExpressions())
        {
            String expr = "(struct_for_each add_name " + form + ")";
            expectArgTypeFailure(expr, 1);
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
        expectContractFailure("(struct \"hello\")");
        expectContractFailure("(struct \"hello\" 13 \"world\")");

        expectArgTypeFailure("(struct 15 14)",0);
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
