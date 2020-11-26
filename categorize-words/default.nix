with import <nixpkgs> {};

( let
    newPlotly = pkgs.python3Packages.buildPythonPackage rec {
      pname = "plotly";
      version = "4.13.0";

      src = pkgs.python3Packages.fetchPypi{
        inherit version; inherit pname;
        sha256 = "0rlw6r771jivgwm4cy175jgpk8c6w10mh95cjryza1rqi3vi9pr0";
      };

      buildInputs = with pkgs.python3Packages; [ decorator nbformat
                                                 pytz requests retrying six];
      doCheck = false;

    };
in pkgs.python3.buildEnv.override rec {
    extraLibs = with pkgs.python3Packages; [
	    matplotlib
      pandas
      jupyter
      scikitlearn
      dominate
      newPlotly
      jupyterlab # Dev
      toolz
      nltk
    ];
  }).env
