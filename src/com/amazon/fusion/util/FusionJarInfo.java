// Copyright (c) 2011-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion.util;

import com.amazon.fusion.FusionException;
import com.amazon.fusion._Private_Trampoline;
import com.amazon.ion.Timestamp;
import java.io.IOException;
import java.io.InputStream;
import java.util.Properties;

/**
 * Provides information about this release of the FusionJava library.
 */
public final class FusionJarInfo
{
    private String ourReleaseLabel;
    private String ourMajorVersion;
    private String ourPackageVersion;
    private Timestamp ourBuildTime;


    /**
     * Constructs a new instance that can provide build information about this
     * library.
     *
     * @throws FusionException if there's a problem loading the build info.
     */
    public FusionJarInfo()
        throws FusionException
    {
        loadBuildProperties();
    }


    /**
     * Gets the public release label of this library.
     * Don't attempt to parse this label; we reserve the right to change its
     * format at any time.
     *
     * @return null if the label is unknown.
     */
    public String getReleaseLabel()
    {
        return ourReleaseLabel;
    }

    /**
     * Gets the Brazil major version of this build, in the form {@code "1.0"}.
     *
     * @return null if the major version is unknown.
     */
    public String getBrazilMajorVersion()
    {
        return ourMajorVersion;
    }

    /**
     * Gets the Brazil package version of this build,
     * in the form {@code "FusionJava-1.0.x.x"}.
     *
     * @return null if the package version is unknown.
     */
    public String getBrazilPackageVersion()
    {
        return ourPackageVersion;
    }

    /**
     * Gets the time at which this package was built.
     *
     * @return null if the build time is unknown.
     */
    public Timestamp getBuildTime()
    {
        return ourBuildTime;
    }

    // TODO writeTo(IonWriter)

    //========================================================================

    /**
     * Gets a property, ensuring a non-empty value.
     * @return null but not empty string
     */
    private String nonEmptyProperty(Properties props, String name)
    {
        String value = props.getProperty(name, "");
        if (value.length() == 0) value = null;
        return value;
    }

    private void loadBuildProperties()
        throws FusionException
    {
        String file = getClass().getSimpleName() + ".properties";
        try
        {
            Properties props = new Properties();

            InputStream in = getClass().getResourceAsStream(file);
            if (in != null)
            {
                try
                {
                    props.load(in);
                }
                finally
                {
                    in.close();
                }
            }

            ourReleaseLabel   = nonEmptyProperty(props, "release_label");
            ourPackageVersion = nonEmptyProperty(props, "brazil_package_version");
            ourMajorVersion   = nonEmptyProperty(props, "brazil_major_version");

            String time = nonEmptyProperty(props, "build_time");
            if (time != null)
            {
                try {
                    ourBuildTime = Timestamp.valueOf(time);
                }
                catch (IllegalArgumentException e)
                {
                    // Badly formatted timestamp. Ignore it.
                }
            }
        }
        catch (IOException e)
        {
            String message = "Unable to load " + file;
            throw _Private_Trampoline.newFusionException(message, e);
        }
    }
}
