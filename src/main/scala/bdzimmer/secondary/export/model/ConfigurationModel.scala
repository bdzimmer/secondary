// Copyright (c) 2016 Ben Zimmer. All rights reserved.

// Case classes for use with project configuration loader and GUI.

package bdzimmer.secondary.export.model

object ConfigurationModel {

  sealed trait ConfigField {
    val key: String
    val default: String
    val description: String
  }

  case class TextConfigField(
      key: String,
      default: String,
      description: String) extends ConfigField

  case class ChooseConfigField(
      key: String,
      default: String,
      choices: List[String],
      description: String) extends ConfigField

  case class BoolConfigField(
      key: String,
      default: String,
      description: String) extends ConfigField

}
