object EMail {
  // injection method
  def apply(user: String, domain: String) = user + "@" + domain

  // extraction method
  def unapply(str: String): Option[(String, String)] = {
    val parts = str split "@"
    if (parts.length == 2) Some(parts(0), parts(1)) else None
  }
}
