// Copyright (c) 2019 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion.junit;

import static com.amazon.fusion.junit.Reflect.assertEqualProperties;
import org.junit.ComparisonFailure;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;


public class ReflectTest
{
    @Rule
    public ExpectedException thrown = ExpectedException.none();


    private static class Bean
    {
        private String s = "default";
        private Integer i;

        public String getS() {
            return s;
        }

        public void setS(String s) {
            this.s = s;
        }

        public Integer getI() {
            return i;
        }

        public void setI(Integer i) {
            this.i = i;
        }
    }


    @Test
    public void testAssertEqualProperties()
    {
        Bean a = new Bean();
        Bean b = new Bean();
        assertEqualProperties(a, b);

        a.setI(12);
        b.setI(12);
        assertEqualProperties(a, b);

        a.setS("foo");

        thrown.expect(ComparisonFailure.class);
        thrown.expectMessage("property s");
        assertEqualProperties(a, b);
    }
}
