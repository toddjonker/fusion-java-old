// Copyright (c) 2019-2024 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion.cli;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import com.amazon.fusion.util.FusionJarInfo;
import com.amazon.ion.IonString;
import com.amazon.ion.IonStruct;
import com.amazon.ion.IonSystem;
import com.amazon.ion.IonTimestamp;
import com.amazon.ion.IonValue;
import com.amazon.ion.Timestamp;
import com.amazon.ion.system.IonSystemBuilder;
import com.amazon.ion.util.JarInfo;
import org.junit.Ignore;
import org.junit.Test;

public class VersionTest
    extends CliTestCase
{
    @Test
    public void testFusionVersion()
        throws Exception
    {
        IonStruct version = getField(loadVersion(), "fusion_version");

        FusionJarInfo info = new FusionJarInfo();

        assertEquals(info.getReleaseLabel(),
                     getString(version, "release_label"));
        assertEquals(info.getBrazilMajorVersion(),
                     getString(version, "brazil_major_version"));
        assertEquals(info.getBrazilPackageVersion(),
                     getString(version, "brazil_package_version"));
        assertEquals(info.getBuildTime(),
                     getTimestamp(version, "build_time"));
    }


    @Test
    public void testIonVersion()
        throws Exception
    {
        IonStruct version = getField(loadVersion(), "ion_version");

        JarInfo info = new JarInfo();

        assertEquals(info.getProjectVersion(),
                     getString(version, "project_version"));
        assertEquals(info.getBuildTime(),
                     getTimestamp(version, "build_time"));
    }


    private IonStruct loadVersion()
        throws Exception
    {
        run("version");

        IonSystem ionSystem = IonSystemBuilder.standard().build();

        // This ensures that the output is pure Ion.
        IonStruct version = (IonStruct) ionSystem.singleValue(stdoutText);
        assertNotNull("No version on stdout", version);
        return version;
    }


    @SuppressWarnings("unchecked")
    private <T extends IonValue> T getField(IonStruct struct, String fieldName)
    {
        IonValue value = struct.get(fieldName);
        assertNotNull("Missing field " + fieldName + " in " + struct, value);
        return (T) value;
    }

    private String getString(IonStruct struct, String fieldName)
    {
        IonString value = getField(struct, fieldName);
        return value.stringValue();
    }

    private Timestamp getTimestamp(IonStruct struct, String fieldName)
    {
        IonTimestamp value = getField(struct, fieldName);
        return value.timestampValue();
    }
}
