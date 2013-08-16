// Copyright (c) 2011-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion.util;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import com.amazon.fusion.FusionException;
import org.junit.Test;

/**
 *
 */
public class FusionJarInfoTest
{
    @Test
    public void testConstruction()
        throws FusionException
    {
        FusionJarInfo info = new FusionJarInfo();

        // When running on a laptop that can't run our Ant targets (due to
        // unavailable Brazil CLI and HappyTrails stuff) this test will fail.
        // Adding -DNOBRAZIL to the Eclipse Installed JRE will allow us to
        // succeed in that case.
        if (System.getProperty("NOBRAZIL") != null) return;

        assertTrue(info.getReleaseLabel().startsWith("R"));
        assertTrue(info.getBrazilMajorVersion().startsWith("1."));
        assertTrue(info.getBrazilPackageVersion().startsWith("FusionJava-1."));
        assertNotNull(info.getBuildTime());
    }
}
