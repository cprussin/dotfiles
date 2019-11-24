{ fetchFromGitHub, epkgs }:

epkgs.trivialBuild rec {
  pname = "zoom-frm";
  src = fetchFromGitHub {
    owner = "cprussin";
    repo = pname;
    rev = "08dd33d2518f4f9a90c804e09baac1db74ffa58f";
    sha256 = "0m6by707jg7zjla7hl9xgma0w3m9apvf5dqdn7yf6qcz354kyqgy";
  };
}
