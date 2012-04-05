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
    {
        assertEval("{}", "(remove {} \"f\")");
        assertEval("{}", "(remove {f:1} \"f\")");
        assertEval("{g:2}", "(remove {g:2,f:1} \"f\")");
        assertEval("{}", "(remove {f:1,g:2} \"f\" (quote g))");
    }

    @Test
    public void testDeepRemove()
    {
        assertEval("{f:1,g:{i:4}}",
                   "(let ((s {f:1,g:{h:3,i:4}}))" +
                   "  (begin" +
                   "    (remove (. s \"g\") \"h\")" +
                   "    s))");
    }

    @Test
    public void testForEachChild()
    {
        eval("(define add_name (func (name value) (add value name)))");
        assertEval("{f:[\"f\"],g:[\"g\"],f:[true,false,\"f\"]}",
                   "(for_each_field add_name " +
                   "  {f:null.list,g:[],f:[true,false]})");
    }
}
