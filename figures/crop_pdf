#!/bin/bash
for FILE in ./*.pdf; do
  pdfcrop --margins '0 0 0 0' "${FILE}"
  rm "${FILE}"
done

for FILE in ./*.pdf; do
  mv "${FILE}" "${FILE%-crop*}.pdf"
done
