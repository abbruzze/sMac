package ucesoft.mac

import ucesoft.mac.misc.Preferences

import java.io.File
import java.util.Properties

/**
 * @author Alessandro Abbruzzetti
 *         Created on 20/12/2024 14:28  
 */
case class ConfigContext(homeDir:File,pref:Preferences,conf:Properties)
