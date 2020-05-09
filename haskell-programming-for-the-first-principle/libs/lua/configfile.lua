username = os.getenv("USER") or "God"
password = os.getenv("PWD") or "dontdisturb"

function getuserpwd (site)
  local cookies = { ["www.a.com"] = {"joe", "god"}
                  , ["www.b.com"] = {"hoe", "godness"}
                  }
  if cookies[site] then
    return cookies[site]
  elseif site:match("[.]c[.]com$") then
    return {"boss", "boss"}
  else
    return { username
           , password }
  end
end
