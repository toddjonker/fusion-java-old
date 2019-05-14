// Copyright (c) 2019 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion.cli;

import static org.junit.Assert.assertEquals;
import com.amazon.fusion.FusionException;
import com.amazon.fusion.util.FusionJarInfo;
import com.amazon.ion.IonString;
import com.amazon.ion.IonStruct;
import com.amazon.ion.IonSystem;
import com.amazon.ion.IonTimestamp;
import com.amazon.ion.Timestamp;
import com.amazon.ion.system.IonSystemBuilder;
import com.amazon.ion.util.JarInfo;
import org.junit.Test;

public class VersionTest
    extends CliTestCase
{
    @Test
    public void testVersionNoArgs()
        throws Exception
    {
        run("version");

        IonSystem ionSystem = IonSystemBuilder.standard().build();

        // This ensures that the output is pure Ion.
        IonStruct result = (IonStruct) ionSystem.singleValue(stdoutText);

        IonStruct version = (IonStruct) result.get("fusion_version");
        checkFusionVersion(version);

        version = (IonStruct) result.get("ion_version");
        checkIonVersion(version);
    }

    private void checkFusionVersion(IonStruct version)
        throws FusionException
    {
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

    private void checkIonVersion(IonStruct version)
    {
        JarInfo info = new JarInfo();

        assertEquals(info.getProjectVersion(),
                     getString(version, "project_version"));
        assertEquals(info.getBuildTime(),
                     getTimestamp(version, "build_time"));
    }

    private String getString(IonStruct struct, String fieldName)
    {
        IonString value = (IonString) struct.get(fieldName);
        return value.stringValue();
    }

    private Timestamp getTimestamp(IonStruct struct, String fieldName)
    {
        IonTimestamp value = (IonTimestamp) struct.get(fieldName);
        return value.timestampValue();
    }
}
