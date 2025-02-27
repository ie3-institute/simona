#!/usr/bin/env bash
set -euo pipefail

# Funktion compareVersionParts vergleicht zwei Versionen basierend auf Branch-Typen.
# Parameter:
#   $1: sourceBranchType (z.B. "hotfix", "feature", "release", "dev")
#   $2: sourceVersion (z.B. "3.1.0")
#   $3: targetBranchType (z.B. "main" oder "dev")
#   $4: targetVersion (z.B. "3.0.0")
#
# Rückgabewert:
#   0 bei gültiger Versionierung, 1 bei Fehler.
compareVersionParts() {
  local sourceBranchType="$1"
  local sourceVersion="$2"
  local targetBranchType="$3"
  local targetVersion="$4"

  # Versionsstrings in Major, Minor, Patch zerlegen:
  IFS='.' read -r s_major s_minor s_patch <<< "$sourceVersion"
  IFS='.' read -r t_major t_minor t_patch <<< "$targetVersion"

  case "$sourceBranchType" in
    hotfix)
      if [ "$targetBranchType" = "main" ]; then
        # Für Hotfix: Major und Minor müssen gleich sein, Patch des Source = Patch des Target + 1
        if [ "$s_major" -eq "$t_major" ] && [ "$s_minor" -eq "$t_minor" ] && [ "$s_patch" -eq $((t_patch+1)) ]; then
          return 0
        else
          echo "Hotfix branch versioning invalid compared to main. (Expected patch: $((t_patch+1)))"
          return 1
        fi
      elif [ "$targetBranchType" = "dev" ]; then
        # Für Hotfix gegen dev: Major und Minor müssen gleich sein und beide Patch-Versionen müssen 0 sein.
        if [ "$s_major" -eq "$t_major" ] && [ "$s_minor" -eq "$t_minor" ] && [ "$s_patch" -eq 0 ] && [ "$t_patch" -eq 0 ]; then
          return 0
        else
          echo "Hotfix branch versioning invalid compared to dev. (Major and minor must be equal and patch 0)"
          return 1
        fi
      else
        echo "Invalid target branch type '$targetBranchType' for hotfix."
        return 1
      fi
      ;;
    feature)
      if [ "$targetBranchType" = "dev" ]; then
        # Für Feature-Branches darf sich die Version nicht ändern: Major und Minor gleich und Patch 0.
        if [ "$s_major" -eq "$t_major" ] && [ "$s_minor" -eq "$t_minor" ] && [ "$s_patch" -eq 0 ] && [ "$t_patch" -eq 0 ]; then
          return 0
        else
          echo "Feature branch versioning invalid compared to dev."
          return 1
        fi
      else
        echo "Feature branches can only be merged into dev."
        return 1
      fi
      ;;
    release)
      if [ "$targetBranchType" = "main" ] || [ "$targetBranchType" = "dev" ]; then
        # Für Release: Patch muss 0 sein und entweder:
        #   - Source Major == Target Major und Source Minor == Target Minor + 1
        #   - oder Source Major == Target Major + 1
        if [ "$s_patch" -ne 0 ]; then
          echo "Release branch patch version must be 0."
          return 1
        fi
        if { [ "$s_major" -eq "$t_major" ] && [ "$s_minor" -eq $((t_minor+1)) ]; } || [ "$s_major" -eq $((t_major+1)) ]; then
          return 0
        else
          echo "Release branch versioning invalid compared to $targetBranchType."
          return 1
        fi
      else
        echo "Release branches can only be merged into main or dev."
        return 1
      fi
      ;;
    dev)
      if [ "$targetBranchType" = "main" ]; then
        # Für dev: Patch muss 0 sein und entweder:
        #   - Source Major == Target Major und Source Minor == Target Minor + 1
        #   - oder Source Major == Target Major + 1
        if [ "$s_patch" -ne 0 ]; then
          echo "Dev branch patch version must be 0."
          return 1
        fi
        if { [ "$s_major" -eq "$t_major" ] && [ "$s_minor" -eq $((t_minor+1)) ]; } || [ "$s_major" -eq $((t_major+1)) ]; then
          return 0
        else
          echo "Dev branch versioning invalid compared to main."
          return 1
        fi
      else
        echo "Dev branch can only be compared with main."
        return 1
      fi
      ;;
    *)
      echo "Invalid source branch type '$sourceBranchType'."
      return 1
      ;;
  esac
}

# Beispiel-Aufruf:
# compareVersionParts "feature" "3.1.0" "dev" "3.1.0"
# if [ $? -eq 0 ]; then
#   echo "Version check passed"
# else
#   echo "Version check failed"
# fi
