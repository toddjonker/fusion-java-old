// Copyright (c) 2012-2014 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.ion.util.IonTextUtils.printString;
import com.amazon.fusion.FusionNumber.SumProc;
import org.junit.Before;
import org.junit.Test;


public class JavaFfiTest
    extends CoreTestCase
{
    @Before
    public void requires()
        throws Exception
    {
        topLevel().requireModule("/fusion/ffi/java");
    }

    public class Uninstantiable extends Procedure
    {
        public Uninstantiable()
            throws Exception
        {
            super("doc");
            throw new Exception("boom");
        }

        @Override
        Object doApply(Evaluator eval, Object[] args)
            throws FusionException
        {
            return null;
        }
    }


    private String name(Class<?> c)
    {
        String className = c.getName();
        return printString(className);
    }


    @Test
    public void testJavaNew()
        throws Exception
    {
        eval("(define plus (java_new " + name(SumProc.class) + "))");
        assertEval(2, "(plus 1 1)");

        eval("(define param (java_new " + name(DynamicParameter.class) + " 1))");
        assertEval(1, "(param)");
    }

    @Test
    public void testJavaNewBadArgs()
        throws Exception
    {
        expectContractExn("(define foo (java_new '''no such class'''))");
        expectContractExn("(define foo (java_new " + name(Uninstantiable.class) + "))");
   }
}
