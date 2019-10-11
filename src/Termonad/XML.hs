{-# LANGUAGE QuasiQuotes #-}

module Termonad.XML where

import Termonad.Prelude

import Data.Default (def)
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
        <property name="title" translatable="yes">Example Application</property>
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
interfaceText = toStrict $ renderText def interfaceDoc

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
              <attribute name="label" translatable="yes">New _Tab</attribute>
              <attribute name="action">app.newtab</attribute>
            </item>
          </section>
          <section>
            <item>
              <attribute name="label" translatable="yes">_Close Tab</attribute>
              <attribute name="action">app.closetab</attribute>
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
            <attribute name="action">app.copy</attribute>
          </item>
          <item>
            <attribute name="label" translatable="yes">_Paste</attribute>
            <attribute name="action">app.paste</attribute>
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
            <attribute name="action">app.find</attribute>
          </item>
          <item>
            <attribute name="label" translatable="yes">Find Above</attribute>
            <attribute name="action">app.findabove</attribute>
          </item>
          <item>
            <attribute name="label" translatable="yes">Find Below</attribute>
            <attribute name="action">app.findbelow</attribute>
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
menuText = toStrict $ renderText def menuDoc

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
aboutText = toStrict $ renderText def aboutDoc

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
closeTabText = toStrict $ renderText def closeTabDoc

preferencesDoc :: Document
preferencesDoc =
  [xmlRaw|
<?xml version="1.0" encoding="UTF-8"?>
<!-- Generated with glade 3.22.1 -->
<interface>
  <requires lib="gtk+" version="3.20"/>
  <object class="GtkDialog" id="preferences">
    <property name="can_focus">False</property>
    <property name="type_hint">dialog</property>
    <child>
      <placeholder/>
    </child>
    <child internal-child="vbox">
      <object class="GtkBox">
        <property name="can_focus">False</property>
        <property name="orientation">vertical</property>
        <property name="spacing">2</property>
        <child internal-child="action_area">
          <object class="GtkButtonBox">
            <property name="can_focus">False</property>
            <property name="layout_style">end</property>
            <child>
              <placeholder/>
            </child>
            <child>
              <object class="GtkButton" id="close">
                <property name="label">gtk-close</property>
                <property name="visible">True</property>
                <property name="can_focus">True</property>
                <property name="receives_default">True</property>
                <property name="use_stock">True</property>
              </object>
              <packing>
                <property name="expand">True</property>
                <property name="fill">True</property>
                <property name="position">1</property>
              </packing>
            </child>
          </object>
          <packing>
            <property name="expand">False</property>
            <property name="fill">False</property>
            <property name="position">0</property>
          </packing>
        </child>
        <child>
          <object class="GtkGrid">
            <property name="visible">True</property>
            <property name="can_focus">False</property>
            <property name="column_homogeneous">True</property>
            <child>
              <object class="GtkFontButton" id="font">
                <property name="visible">True</property>
                <property name="can_focus">True</property>
                <property name="receives_default">True</property>
                <property name="font">Sans 12</property>
                <property name="language">en-us</property>
                <property name="preview_text"/>
              </object>
              <packing>
                <property name="left_attach">1</property>
                <property name="top_attach">0</property>
              </packing>
            </child>
            <child>
              <object class="GtkLabel">
                <property name="visible">True</property>
                <property name="can_focus">False</property>
                <property name="label" translatable="yes">Font:</property>
              </object>
              <packing>
                <property name="left_attach">0</property>
                <property name="top_attach">0</property>
              </packing>
            </child>
            <child>
              <object class="GtkSpinButton" id="scrollbackLen">
                <property name="visible">True</property>
                <property name="can_focus">True</property>
              </object>
              <packing>
                <property name="left_attach">1</property>
                <property name="top_attach">2</property>
              </packing>
            </child>
            <child>
              <object class="GtkLabel">
                <property name="visible">True</property>
                <property name="can_focus">False</property>
                <property name="label" translatable="yes">Scrollback length:</property>
              </object>
              <packing>
                <property name="left_attach">0</property>
                <property name="top_attach">2</property>
              </packing>
            </child>
            <child>
              <object class="GtkCheckButton" id="confirmExit">
                <property name="label" translatable="yes">Confirm exit</property>
                <property name="visible">True</property>
                <property name="can_focus">True</property>
                <property name="receives_default">False</property>
                <property name="draw_indicator">True</property>
              </object>
              <packing>
                <property name="left_attach">0</property>
                <property name="top_attach">3</property>
                <property name="width">2</property>
              </packing>
            </child>
            <child>
              <object class="GtkLabel">
                <property name="visible">True</property>
                <property name="can_focus">False</property>
                <property name="label" translatable="yes">Word char exceptions:</property>
              </object>
              <packing>
                <property name="left_attach">0</property>
                <property name="top_attach">4</property>
              </packing>
            </child>
            <child>
              <object class="GtkEntry" id="wordCharExceptions">
                <property name="visible">True</property>
                <property name="can_focus">True</property>
              </object>
              <packing>
                <property name="left_attach">1</property>
                <property name="top_attach">4</property>
              </packing>
            </child>
            <child>
              <object class="GtkCheckButton" id="showMenu">
                <property name="label" translatable="yes">Show menu</property>
                <property name="visible">True</property>
                <property name="can_focus">True</property>
                <property name="receives_default">False</property>
                <property name="draw_indicator">True</property>
              </object>
              <packing>
                <property name="left_attach">0</property>
                <property name="top_attach">5</property>
                <property name="width">2</property>
              </packing>
            </child>
            <child>
              <object class="GtkLabel">
                <property name="visible">True</property>
                <property name="can_focus">False</property>
                <property name="label" translatable="yes">Cursor blink mode:</property>
              </object>
              <packing>
                <property name="left_attach">0</property>
                <property name="top_attach">7</property>
              </packing>
            </child>
            <child>
              <object class="GtkLabel">
                <property name="visible">True</property>
                <property name="can_focus">False</property>
                <property name="label" translatable="yes">Show tabbar:</property>
              </object>
              <packing>
                <property name="left_attach">0</property>
                <property name="top_attach">6</property>
              </packing>
            </child>
            <child>
              <object class="GtkLabel">
                <property name="visible">True</property>
                <property name="can_focus">False</property>
                <property name="label" translatable="yes">Show scrollbar:</property>
              </object>
              <packing>
                <property name="left_attach">0</property>
                <property name="top_attach">1</property>
              </packing>
            </child>
            <child>
              <object class="GtkComboBoxText" id="showScrollbar">
                <property name="visible">True</property>
                <property name="can_focus">False</property>
              </object>
              <packing>
                <property name="left_attach">1</property>
                <property name="top_attach">1</property>
              </packing>
            </child>
            <child>
              <object class="GtkComboBoxText" id="showTabBar">
                <property name="visible">True</property>
                <property name="can_focus">False</property>
              </object>
              <packing>
                <property name="left_attach">1</property>
                <property name="top_attach">6</property>
              </packing>
            </child>
            <child>
              <object class="GtkComboBoxText">
                <property name="visible">True</property>
                <property name="can_focus">False</property>
              </object>
              <packing>
                <property name="left_attach">1</property>
                <property name="top_attach">7</property>
              </packing>
            </child>
          </object>
          <packing>
            <property name="expand">False</property>
            <property name="fill">True</property>
            <property name="position">1</property>
          </packing>
        </child>
      </object>
    </child>
    <action-widgets>
      <action-widget response="-3">close</action-widget>
    </action-widgets>
  </object>
</interface>
   |]

preferencesText :: Text
preferencesText = toStrict $ renderText def preferencesDoc
