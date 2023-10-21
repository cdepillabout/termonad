{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Termonad.XML where

import Termonad.Prelude

import Data.Default (def)
import Data.FileEmbed (embedFile)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text.Lazy as LText
import Text.XML (renderText)
import Text.XML.QQ (Document, xmlRaw)

-- TODO: A number of widgets have different places where a child can be added
-- (e.g. tabs vs. page content in notebooks). This can be reflected in a UI
-- definition by specifying the “type” attribute on a <child> The possible
-- values for the “type” attribute are described in the sections describing the
-- widget-specific portions of UI definitions.

interfaceDoc :: Document
interfaceDoc =
  [xmlRaw|
    <?xml version="1.0" encoding="UTF-8"?>
    <interface>
      <!-- interface-requires gtk+ 3.8 -->
      <object id="appWin" class="GtkApplicationWindow">
        <property name="title" translatable="yes">Termonad</property>
        <property name="default-width">600</property>
        <property name="default-height">400</property>
        <child>
          <object class="GtkBox" id="content_box">
            <property name="visible">True</property>
            <property name="orientation">vertical</property>
            <child>
              <object class="GtkStack" id="stack">
                <property name="visible">True</property>
              </object>
            </child>
          </object>
        </child>
      </object>
    </interface>
   |]

interfaceText :: Text
interfaceText = LText.toStrict $ renderText def interfaceDoc

menuDoc :: Document
menuDoc =
  [xmlRaw|
    <?xml version="1.0"?>
    <interface>
      <!-- interface-requires gtk+ 3.0 -->
      <menu id="menubar">
        <submenu>
          <attribute name="label" translatable="yes">File</attribute>
          <section>
            <item>
              <attribute name="label" translatable="yes">_New Window</attribute>
              <attribute name="action">app.newwin</attribute>
            </item>
            <item>
              <attribute name="label" translatable="yes">New _Tab</attribute>
              <attribute name="action">win.newtab</attribute>
            </item>
          </section>
          <section>
            <item>
              <attribute name="label" translatable="yes">_Close Tab</attribute>
              <attribute name="action">win.closetab</attribute>
            </item>
            <item>
              <attribute name="label" translatable="yes">_Quit</attribute>
              <attribute name="action">app.quit</attribute>
            </item>
          </section>
        </submenu>
        <submenu>
          <attribute name="label" translatable="yes">Edit</attribute>
          <item>
            <attribute name="label" translatable="yes">_Copy</attribute>
            <attribute name="action">win.copy</attribute>
          </item>
          <item>
            <attribute name="label" translatable="yes">_Paste</attribute>
            <attribute name="action">win.paste</attribute>
          </item>
          <item>
            <attribute name="label" translatable="yes">_Preferences</attribute>
            <attribute name="action">app.preferences</attribute>
          </item>
        </submenu>
        <submenu>
          <attribute name="label" translatable="yes">View</attribute>
          <item>
            <attribute name="label" translatable="yes">_Enlarge Font Size</attribute>
            <attribute name="action">app.enlargefont</attribute>
          </item>
          <item>
            <attribute name="label" translatable="yes">_Reduce Font Size</attribute>
            <attribute name="action">app.reducefont</attribute>
          </item>
        </submenu>
        <submenu>
          <attribute name="label" translatable="yes">Search</attribute>
          <item>
            <attribute name="label" translatable="yes">_Find...</attribute>
            <attribute name="action">win.find</attribute>
          </item>
          <item>
            <attribute name="label" translatable="yes">Find Above</attribute>
            <attribute name="action">win.findabove</attribute>
          </item>
          <item>
            <attribute name="label" translatable="yes">Find Below</attribute>
            <attribute name="action">win.findbelow</attribute>
          </item>
        </submenu>
        <submenu>
          <attribute name="label" translatable="yes">Help</attribute>
          <item>
            <attribute name="label" translatable="yes">_About</attribute>
            <attribute name="action">app.about</attribute>
          </item>
        </submenu>
      </menu>
    </interface>
   |]

menuText :: Text
menuText = LText.toStrict $ renderText def menuDoc

aboutDoc :: Document
aboutDoc =
  [xmlRaw|
    <?xml version="1.0"?>
    <interface>
    <!-- interface-requires gtk+ 3.8 -->
      <object id="aboutDialog" class="GtkDialog">
        <property name="title" translatable="yes">About</property>
        <property name="resizable">False</property>
        <property name="modal">True</property>
        <child internal-child="vbox">
          <object class="GtkBox" id="vbox">
            <child>
              <object class="GtkGrid" id="grid">
                <property name="visible">True</property>
                <property name="margin">6</property>
                <property name="row-spacing">12</property>
                <property name="column-spacing">6</property>
                <child>
                  <object class="GtkLabel" id="fontlabel">
                    <property name="visible">True</property>
                    <property name="label">_Font:</property>
                    <property name="use-underline">True</property>
                    <property name="mnemonic-widget">font</property>
                    <property name="xalign">1</property>
                  </object>
                  <packing>
                    <property name="left-attach">0</property>
                    <property name="top-attach">0</property>
                  </packing>
                </child>
                <child>
                  <object class="GtkFontButton" id="font">
                    <property name="visible">True</property>
                  </object>
                  <packing>
                    <property name="left-attach">1</property>
                    <property name="top-attach">0</property>
                  </packing>
                </child>
                <child>
                  <object class="GtkLabel" id="transitionlabel">
                    <property name="visible">True</property>
                    <property name="label">_Transition:</property>
                    <property name="use-underline">True</property>
                    <property name="mnemonic-widget">transition</property>
                    <property name="xalign">1</property>
                  </object>
                  <packing>
                    <property name="left-attach">0</property>
                    <property name="top-attach">1</property>
                  </packing>
                </child>
                <child>
                  <object class="GtkComboBoxText" id="transition">
                    <property name="visible">True</property>
                    <items>
                      <item translatable="yes" id="none">None</item>
                      <item translatable="yes" id="crossfade">Fade</item>
                      <item translatable="yes" id="slide-left-right">Slide</item>
                    </items>
                  </object>
                  <packing>
                    <property name="left-attach">1</property>
                    <property name="top-attach">1</property>
                  </packing>
                </child>
              </object>
            </child>
          </object>
        </child>
      </object>
    </interface>
   |]

aboutText :: Text
aboutText = LText.toStrict $ renderText def aboutDoc

closeTabDoc :: Document
closeTabDoc =
  [xmlRaw|
    <?xml version="1.0"?>
    <interface>
    <!-- interface-requires gtk+ 3.8 -->
      <object id="closeTabDialog" class="GtkDialog">
        <property name="title" translatable="yes">Close Tab</property>
        <property name="resizable">False</property>
        <property name="modal">True</property>
        <child internal-child="vbox">
          <object class="GtkBox" id="vbox">
            <property name="hexpand">True</property>
            <property name="margin">10</property>
            <property name="vexpand">True</property>
            <child>
              <object class="GtkLabel">
                <property name="hexpand">True</property>
                <property name="label">Close tab?</property>
                <property name="margin">10</property>
                <property name="vexpand">True</property>
                <property name="visible">True</property>
              </object>
            </child>
            <child>
              <object class="GtkButtonBox">
                <property name="hexpand">True</property>
                <property name="margin">10</property>
                <property name="vexpand">True</property>
                <property name="visible">True</property>
                <child>
                  <object class="GtkButton">
                    <property name="label">Yes, close tab</property>
                    <property name="visible">True</property>
                    <property name="can_focus">True</property>
                    <property name="relief">GTK_RELIEF_NORMAL</property>
                  </object>
                </child>
                <child>
                  <object class="GtkButton">
                    <property name="label">No, do NOT close tab</property>
                    <property name="visible">True</property>
                    <property name="can_focus">True</property>
                    <property name="relief">GTK_RELIEF_NORMAL</property>
                  </object>
                </child>
              </object>
            </child>
          </object>
        </child>
      </object>
    </interface>
   |]

closeTabText :: Text
closeTabText = LText.toStrict $ renderText def closeTabDoc

preferencesText :: Text
preferencesText = decodeUtf8 $(embedFile "glade/preferences.glade")
